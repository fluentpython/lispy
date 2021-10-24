/* $Id: rt.c,v 4.0 1995/07/10 06:52:26 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* A small run-time library for chap6f.					*/

#include "rt.h"

/***********************************************************************
 * Errors are simply coded by numbers.
 *   1        : initialization
 *  20        : system, apply, call/cc
 *  30        : conversions
 *  40        : float arithmetic
 *  50        : arithmetic for fixnums
 *  60        : list 
 *  70        : output 
 *  80        : char 
 *  90        : string
 * 100        : allocation error.
 * 200        : invokers
									*/

#ifdef GC_STOP_AND_COPY
/***********************************************************************
 * A simple stop and copy GC.
 * The space is made of the low one and the high one which are used
 * alternatively. Allocations are performed in the current space which 
 * initially is the low one.
 * CAUTION: this code supposes the two spaces to be a contiguous array
 *          of bytes without holes. FUTURE.
 * zones_number is the number of allocated zones (with unit SCM_ZONE_SIZE).
 * Zone_Start is the beginning of the lower space.
 * Zone_End   is the end of the higher space.
 * zone_index points to the first free address in the current space.
 * zone_copied points to the first object of the new space that might have
 *         sons that are not yet in the new space.
 * zone_limit is the end of the current space.
 * zone_middle is the border between the two spaces.
 * zone_static_end refers to the lower space and 
 *         anything under is considered static and cannot be moved.
 * low2high specifies the way objects are copied. By the way, it also
 * 	   indicates if LOW2HIGH that the current space is the low one.	
 * gc_state forces gc_neededp to answer `yes'
 * SCM_gc_number counts the number of gc
 * max_used_space is the maximal space used.	
 * SCM_roots contains the things that must be preserved by the GC. 
 *           Caution: It is a real object of type ROOT.			*/

static SIZE_T zones_number = 0 ;
static char   *Zone_Start ;
static char   *Zone_End ;
static char   *zone_index ;
static char   *zone_copied ;
static char   *zone_limit ;
static SCM    zone_middle ;
static SCM    zone_static_end ;
static enum { LOW2HIGH=1, HIGH2LOW } way = LOW2HIGH ;
static SCM_gc_state gc_state = GC_NOT_NEEDED ;
static SIZE_T SCM_gc_number = 0 ;
static SIZE_T max_used_space = 0 ;
SCM SCM_roots ;

/* Return a zone or fail. The size is given with respect to the 
 * SCM_ZONE_SIZE unit. It is only possible to extend the lower space.
 * To maintain the equality of the two spaces, two zones are allocated.	
 * This routine only updates the boundaries of the total space, it does 
 * not update zone_limit nor zone_middle. 				*/
static void *allocate_zone (SIZE_T factor)
{ char *new_zone ;
  new_zone = (char*) MALLOC(2*factor*SCM_ZONE_SIZE) ;
  if ( new_zone == (char*) NULL ) SCM_error(113) ;
  zones_number = zones_number + factor ;
  Zone_End = Zone_End + 2*factor*SCM_ZONE_SIZE ;
  return (void*) new_zone ;
}

/* The replacement for malloc.						*/
void *allocate_object (SIZE_T size)
{ char *result = zone_index ;
  char *new_index = zone_index + size ;
  if ( new_index < zone_limit )
    { zone_index = new_index ;
      return (void*) result ; } ;
  /* CAUTION We suppose the two spaces to be contiguous without holes.
     We thus extend the heap waiting for a gc to compact the heap. */
  { SIZE_T zone_size = ((SIZE_T) 1) + (size/((SIZE_T) SCM_ZONE_SIZE)) ;
    char *new_zone = (char*) allocate_zone(zone_size) ;
    /* Zone_End is already updated, Zone_Start, zone_index do not change. */
    zone_limit = zone_limit + zone_size*SCM_ZONE_SIZE ;
    /* It is simpler to extend the lower space. For the higher space
     zone_middle will be computed after the next gc. */
    if ( way==LOW2HIGH ) zone_middle = (SCM) zone_limit ;
    /* We set gc_state so that a gc will be performed at the next
       task switch. */
    gc_state = GC_NEEDED ;
    return allocate_object(size) ; } /* SPEED future */
}

/* Initialize the allocator, just allocate the first zone.		*/
static void initialize_allocator()
{ SIZE_T initial_factor = 2 ;
  Zone_Start = (char*) allocate_zone((SIZE_T) initial_factor) ;
  Zone_End = Zone_Start + 2*initial_factor*SCM_ZONE_SIZE ;
  zone_index = Zone_Start ;
  zone_middle = (SCM) SCM_mid_address(Zone_Start,Zone_End) ;
  zone_limit = (char*) zone_middle ;
  /* zone_static_end will be updated after SCM_initialize(). */
  zone_static_end = (SCM) Zone_Start ;
}

/* This function is called after static initializations are finished.
 * It sets the static space limit, then resets the two spaces to 
 * a similar size.							*/
void staticize_allocator()
{ SIZE_T delta ;
  zone_static_end = (SCM) zone_index ;
  zone_middle = (SCM) SCM_mid_address(zone_static_end,Zone_End) ;
  zone_limit = (char*) zone_middle ;
}

/* Reports some statistics on the evaluation				*/
void SCM_report_usage () {
   fprintf(stderr,"Usable data space= %s%u*%dK.\n",
           "(twice) ", zones_number, SCM_ZONE_SIZE/1024 ) ;
   { SIZE_T used_space ;
     if ( way==HIGH2LOW ) /* we are in the higher space */
       used_space = zone_index - (char*) zone_middle ;
     else used_space = zone_index - Zone_Start ;
     /* This is unfair since used_space is not compacted. */
     max_used_space = (used_space<max_used_space)?max_used_space:used_space ;
     fprintf(stderr,"Maximal used data space= %uK.\n",
             1+(max_used_space/SCM_ZONE_SIZE) ) ;
   } ;
   fprintf(stderr,"Performed %u GC.\n",SCM_gc_number) ;
   }

/* Decide if it is time to perform a GC.
 * It is when there is no more room free or if an allocation was made after
 * the last GC (which turned the gc_state ON.				*/
SCM_gc_state SCM_gc_neededp (SIZE_T room)
{ if ( (zone_limit - zone_index) < room )
    return GC_NEEDED ;
  else return gc_state ;
}

/* Return the C length of a SCM object.					*/
static SIZE_T SCM_size (SCM o)
{ switch (SCM_tag_of(o)) {
 case SCM_PAIR_TAG:     return SIZEOF(struct SCM_pair) ;
 case SCM_SYMBOL_TAG:   return SIZEOF(struct SCM_symbol) ;
 case SCM_EMPTY_TAG: 
 case SCM_UNDETERMINED_TAG: 
 case SCM_FALSE_TAG:
 case SCM_EOF_TAG:
 case SCM_TRUE_TAG:     return SIZEOF(struct SCM_immediate_object) ; 
 case SCM_CLOSURE_TAG: {
   SIZE_T length = (o->closure).size ;
   return ( SIZEOF(struct SCM_closure) 
           + ( SIZEOF(SCM) * ((length>0)?(length-1):0) ) ) ;
 } ;
 case SCM_PRIMITIVE_TAG: return SIZEOF(struct SCM_primitive) ;
 case SCM_FIXNUM_TAG:    return SIZEOF(struct SCM_fixnum) ;
 case SCM_BOX_TAG:       return SIZEOF(struct SCM_box) ;
 case SCM_STRING_TAG: {
   SIZE_T size = SCM_string2size(o) ;
   if ( SCM_mutable_stringp(o) )
     return SIZEOF(struct SCM_string) + ( size * SIZEOF(char) ) ;
   else return SIZEOF(struct SCM_string) ;
 } ;
 case SCM_CHAR_TAG:       return SIZEOF(struct SCM_char) ;
 case SCM_PORT_TAG:       return SIZEOF(struct SCM_port) ;
 case SCM_FLOATNUM_TAG:   return SIZEOF(struct SCM_floatnum) ;
 case SCM_STACK_SLICE_TAG: {
   SIZE_T size = (o->stack_slice).stack_slice_length ;
   return ( SIZEOF(struct SCM_stack_slice) + size*SIZEOF(char) ) ;
 } ;
 case SCM_CONTINUATION_TAG: return SIZEOF(struct SCM_continuation) ;
 case SCM_PROCESS_TAG: {
   SIZE_T length = (o->process).size ;
   return ( SIZEOF(struct SCM_process) 
           + ( SIZEOF(SCM) * ((length>1)?(length-2):0) ) ) ;
 } ;
 case SCM_MODULE_TAG: {
   SIZE_T length = (o->module).size ;
   return ( SIZEOF(struct SCM_module)
           + ( SIZEOF(SCM) * ((length>0)?(length-1):0) ) ) ;
 } ;
 case SCM_ROOT_TAG: {
   SIZE_T length = (o->root).size ;
   return ( SIZEOF(struct SCM_root)
           + ( SIZEOF(SCM) * ((length>0)?(length-1):0) ) ) ;
 } ;
 case SCM_FRAME_TAG: {
   SIZE_T length = (o->frame).size ;
   return ( SIZEOF(struct SCM_frame)
           + ( SIZEOF(SCM) * ((length>0)?(length-1):0) ) ) ;
 } ;
 case SCM_FUNCTION_TAG:       return SIZEOF(struct SCM_function) ;
 default: SCM_error(123) ;
 } ;
} 

/* Move an object referenced by o from the old space to the new space if
 * not already done and if not static. In any case return the address that
 * the object would have after the GC.					*/
static SCM SCM_move (SCM o)
{ if ( (o==(SCM) NULL)
      || (way==LOW2HIGH)
         ?((o>=zone_middle)||(o<zone_static_end))
         :(o<zone_middle) )
    /* the object is already in the new space or is a static object 
     CAUTION: this imposes that static objects must be immutable. */
    return o ;
  else 
    if ( ((o->object).header.link) != (SCM) NULL )
      /* The object has already been moved to the new space */
      return ((o->object).header.link) ;
    else 
      { SCM new = (SCM) zone_index ;
        zone_index = zone_index + SCM_size(o) ;
        /* This error can only appear if HIGH2LOW, the higher space was
           so much extended after the last GC that the amount of used
           objects is greater than the size of the lower space. */
        if ( (way==HIGH2LOW) && (zone_index >= (char*) zone_middle) ) 
          SCM_error(114) ;
        (o->object).header.link = new ;
        (new->object).header.link = (SCM) NULL ;
        SCM_tag_of(new) = SCM_tag_of(o) ;
        switch (SCM_tag_of(o)) {
        case SCM_PAIR_TAG: {
          SCM_cdr(new) = (SCM_cdr(o)) ;
          SCM_car(new) = (SCM_car(o)) ;
          break ;
        } ;
        case SCM_SYMBOL_TAG: {
          SCM_symbol2string(new) = (SCM_symbol2string(o)) ;
          break ;
        } ;
        case SCM_EMPTY_TAG: 
        case SCM_UNDETERMINED_TAG: 
        case SCM_FALSE_TAG:
        case SCM_EOF_TAG:
        case SCM_TRUE_TAG: {
          SCM_error(124) ; /* always unique and static */
        } ;
        case SCM_CLOSURE_TAG: {
          unsigned int i ;
          SIZE_T length ;
          (new->closure).behavior = (o->closure).behavior ;
          (new->closure).arity = (o->closure).arity ;
          length = (new->closure).size = (o->closure).size ;
          for ( i=0 ; i<((length>0)?length:1) ; i++ )
            SCM_closed_value(new,i) = (SCM_closed_value(o,i)) ;
          break ; 
        } ;
        case SCM_PRIMITIVE_TAG: {
          (new->primitive).behavior = (o->primitive).behavior ;
          (new->primitive).arity = (o->primitive).arity ;
          SCM_primitive2name(new) = (SCM_primitive2name(o)) ;
          break ;
        } ;
        case SCM_FIXNUM_TAG: {
          SCM_fixnum2int(new) = SCM_fixnum2int(o) ;
          break ;
        } ;
        case SCM_BOX_TAG: {
          SCM_box_content(new) = (SCM_box_content(o)) ;
          break ;
        } ;
        case SCM_STRING_TAG: {
          SIZE_T size ;
          size = SCM_string2size(new) = SCM_string2size(o) ;
          SCM_string2Cstring(new) = SCM_string2Cstring(o) ;
          if ( SCM_mutable_stringp(o) )
            { SIZE_T i ;
              for ( i=0 ; i<size ; i++ )
                ((new->string).C_string)[i] = ((o->string).C_string)[i] ;
            } ;
          break ;
        } ;
        case SCM_CHAR_TAG: {
          (new->character).C_char = (o->character).C_char ;
          break ;
        } ;
        case SCM_PORT_TAG: {
          (new->port).flag = (o->port).flag ;
          SCM_port2poken_char(new) = (SCM_port2poken_char(o)) ;
          (new->port).file = (o->port).file ;
          break ;
        } ;
        case SCM_FLOATNUM_TAG: {
          SCM_floatnum2float(new) = SCM_floatnum2float(o) ;
          break ;
        } ;
        case SCM_STACK_SLICE_TAG: {
          int i ;
          SIZE_T size ;
          SCM_report_error(121,__FILE__,__LINE__) ; /* Not yet implemented */
          break ;
        } ;
        case SCM_CONTINUATION_TAG: {
          SCM_continuation2transformer(new) = 
            (SCM_continuation2transformer(o)) ;
          SCM_continuation2below(new)       = 
            (SCM_continuation2below(o)) ;
          break ;
        } ;
        case SCM_MODULE_TAG: {
          SCM_error(128) ; /* Should never move! */
        } ;
        case SCM_ROOT_TAG: {
          SCM_error(129) ; /* Should never move! */
        } ;
        case SCM_FRAME_TAG: {
          unsigned int i ;
          SIZE_T length ;
          (new->frame).next = (o->frame).next ;
          length = (new->frame).size = (o->frame).size ;
          for ( i=0 ; i<((length>0)?length:1) ; i++ )
            (new->frame).value[i] = (o->frame).value[i] ;
          break ; 
        } ;
        case SCM_FUNCTION_TAG: {
          (new->function).behavior = (o->function).behavior ;
          (new->function).environment = (o->function).environment ;
          break ;
        } ;
        default: SCM_error(122) ; } ;
        return new ; } ;
}

/* Move the sons of an object to the new space. The object itself can
 * be in the new space or be static. Static objects are immutable so
 * it is useless to trace them anymore.					*/
static SCM SCM_move_sons (SCM o)
{ switch (SCM_tag_of(o)) {
 case SCM_PAIR_TAG: {
   SCM_cdr(o) = SCM_move(SCM_cdr(o)) ;
   SCM_car(o) = SCM_move(SCM_car(o)) ;
   break ;
 } ;
 case SCM_SYMBOL_TAG: {
   SCM_symbol2string(o) = SCM_move(SCM_symbol2string(o)) ;
   break ;
 } ;
 case SCM_EMPTY_TAG: 
 case SCM_UNDETERMINED_TAG: 
 case SCM_FALSE_TAG:
 case SCM_EOF_TAG:
 case SCM_TRUE_TAG: {
   SCM_error(125) ;             /* always unique and static */
 } ;
 case SCM_CLOSURE_TAG: {
   unsigned int i ;
   SIZE_T length ;
   length = (o->closure).size ;
   for ( i=0 ; i<((length>0)?length:1) ; i++ )
     SCM_closed_value(o,i) = SCM_move(SCM_closed_value(o,i)) ;
   break ; 
 } ;
 case SCM_PRIMITIVE_TAG: {
   SCM_primitive2name(o) = SCM_move(SCM_primitive2name(o)) ;
   break ;
 } ;
 case SCM_FIXNUM_TAG: {
   break ;
 } ;
 case SCM_BOX_TAG: {
   SCM_box_content(o) = SCM_move(SCM_box_content(o)) ;
   break ;
 } ;
 case SCM_STRING_TAG: {
   break ;
 } ;
 case SCM_CHAR_TAG: {
   break ;
 } ;
 case SCM_PORT_TAG: {
   SCM_port2poken_char(o) = SCM_move(SCM_port2poken_char(o)) ;
   break ;
 } ;
 case SCM_FLOATNUM_TAG: {
   break ;
 } ;
 case SCM_STACK_SLICE_TAG: {
   int i ;
   SIZE_T size ;
   SCM_report_error(126,__FILE__,__LINE__) ; /* Not yet implemented */
   break ;
 } ;
 case SCM_CONTINUATION_TAG: {
   SCM_continuation2transformer(o) = 
     SCM_move(SCM_continuation2transformer(o)) ;
   SCM_continuation2below(o)       = 
     SCM_move(SCM_continuation2below(o)) ;
   break ;
 } ;
 case SCM_MODULE_TAG: {
   SIZE_T i, length ;
   length = (o->module).size ;
   for ( i=0 ; i<((length>0)?length:1) ; i++ )
     (o->module).global[i] = SCM_move((o->module).global[i]) ;
   break ;
 } ;
 case SCM_ROOT_TAG: {
   SIZE_T i, length ;
   length = (o->root).size ;
   for ( i=0 ; i<((length>0)?length:1) ; i++ )
     SCM_move_sons((o->root).module[i]) ;
   break ;
 } ;
 case SCM_FRAME_TAG: {
   SIZE_T i, length ;
   length = (o->frame).size ;
   SCM_move_sons((o->frame).next) ;
   for ( i=0 ; i<((length>0)?length:1) ; i++ )
     SCM_move_sons((o->frame).value[i]) ;
   break ;
 } ;
 case SCM_FUNCTION_TAG: {
   SCM_move_sons((o->function).environment) ;
   break ;
 } ;
 default: SCM_error(127) ; } ;
  return o ; 
}

/* Start a GC. roots represent the state of the computation that must
 * be preserved, n is the number of roots in roots. The roots array
 * is updated in place to point to the moved objects.			*/
void SCM_gc (unsigned int n, SCM roots[] /*in-out*/)
{ unsigned int i ;
  SIZE_T used_space, usable_space, excess ;
  ++SCM_gc_number ; 
  if ( way==LOW2HIGH )
    /* The old space is the lower space */
    {  zone_copied = zone_index = (char*) zone_middle ;
      zone_limit = Zone_End ; }
  else {  zone_copied = zone_index = (char*) zone_static_end ;
         zone_limit = (char*) zone_middle ; } ;
  /* Move roots and adjust C global variables 
     ie the list of processes, the current computation and the 
     global variables of all modules. */
  SCM_move_sons(SCM_roots) ;
  for ( i=0 ; i<n ; i++ )
    roots[i] = SCM_move(roots[i]) ;
  /* Iterate on the new space until completion */
  while ( zone_copied < zone_index )
    { SCM o = (SCM) zone_copied ;
      SCM_move_sons(o) ;
      zone_copied = zone_copied + SCM_size(o) ; } ;      
  if ( way==HIGH2LOW )
    { /* We just finished copying everything in the lower space.
         We try to extend the heap if the use ratio is too high. */
      SIZE_T factor ;
      /* Recompute zone_middle which may have been changed
         if the higher space was extended after the previous gc. */
      zone_middle = (SCM) SCM_mid_address(zone_static_end,Zone_End) ;
      zone_limit = (char*) zone_middle ;
      used_space = (zone_index - (char*) zone_static_end) ;
      usable_space = (zone_limit - (char*) zone_static_end) ;
      /* Appel said that the usable space must be at least 8 times larger
         than the used space. Extend the lower space if necessary. */
      if ( used_space*SCM_GC_EXTENSION_THRESHOLD > usable_space )
        { factor = 1 + (used_space*SCM_GC_EXTENSION_THRESHOLD - 
                         usable_space)/SCM_ZONE_SIZE ;
          allocate_zone (factor) ; 
          zone_middle = (SCM) SCM_mid_address(zone_static_end,Zone_End) ;
          zone_limit = (char*) zone_middle ; } ;
    }
  else { /* everything is copied in the higher space. */
         used_space = (zone_index - (char*) zone_middle) ; } ;
  max_used_space = (used_space<max_used_space)?max_used_space:used_space ;
  way = ((way==LOW2HIGH)?HIGH2LOW:LOW2HIGH) ;
  gc_state = GC_NOT_NEEDED ;
}
#endif /* GC_STOP_AND_COPY */

#ifdef NO_GC
/***********************************************************************
 * A simple preallocator to factorize calls to malloc.
 * zones_number: number of allocated zones.
 * zone_index: points to the space where the next object will be allocated
 *             (if there is enough space).
 * zone_end: limit of the higher zone.					*/
static SIZE_T zones_number = 0 ;
static char   *zone_index ;
static char   *zone_end ;
static SCM    zone_static_end ;

/* Return a zone or fail. The size is given with respect to the 
 * SCM_ZONE_SIZE unit.							*/
static void *allocate_zone (SIZE_T factor)
{ zone_index = (char*) MALLOC(factor*SCM_ZONE_SIZE) ;
  if ( zone_index == (char*) NULL ) SCM_error(113) ;
  zones_number = zones_number + factor ;
  zone_end = zone_index + factor*SCM_ZONE_SIZE ;
  return (void*) zone_index ;
}

/* The replacement for malloc.						*/
void *allocate_object (SIZE_T size)
{ char *result = zone_index ;
  char *new_index = zone_index + size ;
  if ( new_index < zone_end )
    { zone_index = new_index ;
      return (void*) result ; } ;
  allocate_zone(((SIZE_T) 1) + (size/((SIZE_T) SCM_ZONE_SIZE))) ;
  return allocate_object(size) ;
}

/* Initialize the allocator, just allocate the first zone.		*/
static void initialize_allocator()
{ allocate_zone((SIZE_T) 1) ;
}

void staticize_allocator() {
}

/* Reports some statistics on the evaluation				*/
void SCM_report_usage () {
   fprintf(stderr,"Allocated data size= %s%u*%dK.\n", 
           "", zones_number, SCM_ZONE_SIZE/1024 ) ;
}
#endif /* NO_GC */

/***********************************************************************
 * ALLOCATORS: 
 *   All objects are boxed to simplify. They are named SCM_make_XXX
 *   or SCM_allocate_XXX. Errors are numbered from 100.
 *
 * Allocates a general Scheme object with a precise tag and size SCM slots 
 * sequentially after. You must cast the result and initialize
 * the other slots.							*/

SCM SCM_allocate (SCM_TAG tag, SIZE_T size)
{ SCM result ;
  result = (SCM) allocate_object( SIZEOF(tag) + (size*SIZEOF(SCM)) ) ;
  if ( result == (SCM) NULL) SCM_error(100) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  return result ;
}

/* Allocate a fixnum, try to find it in the preallocated ones.
 * SCM_make_fixnum appears in C generated code, SCM_allocate_fixnum 
 * is used in SCM_initialize(). 					*/

struct SCM_fixnum SCM_small_fixnums[SCM_SMALL_FIXNUM_LIMIT] ;

SCM SCM_allocate_fixnum (int value)
{ SCM result ;
  SCM_TAG tag = SCM_FIXNUM_TAG ;
  result = (SCM) allocate_object( SIZEOF(struct SCM_fixnum) ) ;
  if ( result == (SCM) NULL) SCM_error(101) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->fixnum).value = value ;
  return result ;
}

SCM SCM_make_fixnum (int value)
{ if ((value >= 0) & (value < SCM_SMALL_FIXNUM_LIMIT))
    return ((SCM) (&(SCM_small_fixnums[value]))) ; 
  else return (SCM_allocate_fixnum(value)) ;
}

/* Allocate a floatnum							*/

SCM SCM_make_floatnum (float value)
{ SCM result ;
  SCM_TAG tag = SCM_FLOATNUM_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_floatnum)) ;
  if ( result == (SCM) NULL) SCM_error(102) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->floatnum).value = value ;
  return result ;
}

/* Allocate a pair							*/

SCM SCM_make_pair (SCM car, SCM cdr)
{ SCM result ;
  SCM_TAG tag = SCM_PAIR_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_pair)) ;
  if ( result == (SCM) NULL) SCM_error(103) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->pair).car = car ;
  (result->pair).cdr = cdr ;
  return result ;
}
  
/* Push a frame t onto a continuation q.				*/

SCM SCM_make_continuation (SCM t, SCM q)
{ SCM result ;
  SCM_TAG tag = SCM_CONTINUATION_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_continuation)) ;
  if ( result == (SCM) NULL) SCM_error(113) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->continuation).transformer = t ;
  (result->continuation).below = q ;
  return result ;
}
  
/* Allocate a box.
 * Boxes serves to hold values in case of mutable variables.		*/

SCM SCM_make_box (SCM content)
{ SCM result ;
  SCM_TAG tag = SCM_BOX_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_box)) ;
  if ( result == (SCM) NULL) SCM_error(104) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->box).content = content ;
  return result ;
}

/* Allocate a symbol.
 * This always allocate a new symbol without considering other existing
 * symbols. Hash tables for that are handled in Scheme.			*/

SCM SCM_make_symbol (SCM string)
{ SCM result ;
  SCM_TAG tag = SCM_SYMBOL_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_symbol)) ;
  if ( result == (SCM) NULL) SCM_error(105) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->symbol).string = string ;
  return result ;
}

/* Allocate a string.
 * A Scheme string created from a C string is immutable and has the
 * the size of the C string. It is rather a `rope'.
 * A Scheme string can also be allocated with a given size and filled
 * or modified later. In that case the characters follow the header.
 * It is also possible to insert the character zero. All the characters
 * are output when printed.						*/

SCM SCM_make_string (char *C_string)
{ SCM result ;
  SCM_TAG tag = SCM_STRING_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_string)) ;
  if ( result == (SCM) NULL) SCM_error(106) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->string).size = STRLEN(C_string) ;
  (result->string).C_string = C_string ;
  return result ;
}

SCM SCM_allocate_string (SIZE_T size)
{ SCM result ;
  SCM_TAG tag = SCM_STRING_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_string) +
                                 (size*SIZEOF(char)) ) ;
  if ( result == (SCM) NULL) SCM_error(107) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->string).size = size ;
  (result->string).C_string = ((char*) (result->string).C_string)+1 ;
  return result ;
}

/* Allocate a character. A global table contains the first
 * character codes. This makes easy to convert an integer into a 
 * Scheme character.							*/

struct SCM_char SCM_characters[SCM_CHAR_SIZE] ;

SCM SCM_make_character (char C_char)
{ SCM result ;
  SCM_TAG tag = SCM_CHAR_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_char)) ;
  if ( result == (SCM) NULL) SCM_error(108) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->character).C_char = C_char ;
  return result ;
}

/* Allocate a primitive 
 * To ease debugging, the name of the primitive is also given. Any legal
 * Scheme object can be a name (for instance an original Sexp).		*/

SCM SCM_make_primitive (SCM (*address)(), int arity, SCM name)
{ SCM result ;
  SCM_TAG tag = SCM_PRIMITIVE_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_primitive)) ;
  if ( result == (SCM) NULL) SCM_error(109) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->primitive).behavior = address ;
  (result->primitive).arity = arity ;
  (result->primitive).name = name ;
  return result ;
}

/* Allocate a closure with size slots to hold the closed environment.	*/

SCM SCM_make_closure (SCM (*address)(), int arity, SIZE_T size)
{ SCM result ;
  SCM_TAG tag = SCM_CLOSURE_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_closure) + 
                                 (SIZEOF(SCM)*((size>1)?(size-1):0)) ) ;
  if ( result == (SCM) NULL) SCM_error(110) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->closure).behavior = address ;
  (result->closure).arity = arity ;
  (result->closure).size = size ;
  (result->closure).environment[0] = (SCM) NULL ;
  return result ;
}

/* Allocate a port. 
 * There is a slot for the poken character. This slot initially holds
 * (), can contain a SCM character or the Scheme object for
 * end-of-file.								*/

SCM SCM_make_port (FILE *file, int flag)
{ SCM result ;
  SCM_TAG tag = SCM_PORT_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_port)) ;
  if ( result == (SCM) NULL) SCM_error(111) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->port).file = file ;
  (result->port).flag = flag ;
  (result->port).poken_character = SCM_empty ;
  return result ;
}

/* Allocate a stack slice with size bytes long.
 * This is used to hold copies of stack for a naive implementation of
 * call/cc (written in pure C and not guaranteed to be portable).	
 * This type of data is used in direct style to implement call/cc.	*/

SCM SCM_make_stack_slice (SIZE_T size)
{ SCM result ;
  SCM_TAG tag = SCM_STACK_SLICE_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_stack_slice)
                                 +(size*SIZEOF(char)) ) ;
  if ( result == (SCM) NULL ) SCM_error(112) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  return result ;
}

/* Allocate the unique root object */
SCM SCM_make_root (SIZE_T size)
{ SCM result ;
  SCM_TAG tag = SCM_ROOT_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_root)
                                 + (SIZEOF(SCM)*((size>1)?(size-1):0)) ) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->root).size = size ;
  return result ;
}

/* Chap6fc represents all invokable objects by functions */
SCM SCM_make_function (SCM (*Cfunction)(), SCM environment) {
  SCM result ;
  SCM_TAG tag = SCM_FUNCTION_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_function)) ;
  if ( result == (SCM) NULL ) SCM_error(110) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->function).behavior = Cfunction ;
  (result->function).environment = environment ;
  return result ;
}

/* Allocate a frame to hole size values */
SCM SCM_make_frame (SIZE_T size)
{ SCM result ;
  SCM_TAG tag = SCM_FRAME_TAG ;
  result = (SCM) allocate_object(SIZEOF(struct SCM_frame)
                                 + (SIZEOF(SCM)*size) ) ;
  SCM_tag_of(result) = tag ;
  (result->object).header.link = (SCM) NULL ;
  (result->frame).size = size ;
  return result ;
}

/***********************************************************************
 * Time: Get the user time and turns into a float expressed in seconds.	*/
/* Note change this name ??? */

SCM SCM_Get_Universal_Time ()
{ static struct rusage rusage ;
  static long seconds, microseconds ;
  float delta ;
  if ( 0==getrusage(RUSAGE_SELF,&rusage) ) 
    { seconds = (rusage.ru_utime).tv_sec ;
      microseconds = (rusage.ru_utime).tv_usec ;
      delta = ((float) seconds) + (((float) microseconds) / 10e+6) ;
      return SCM_make_floatnum(delta) ;
    } 
  else SCM_error(29) ;
}

/***********************************************************************
 * Predicates								*/

#define DefMonadicPred(name,operator)	\
SCM name (SCM x)		\
{ return (SCM_2bool(operator(x))) ;	\
}

DefMonadicPred(SCM_PairP,SCM_pairp)
DefMonadicPred(SCM_NullP,SCM_nullp)
DefMonadicPred(SCM_FixnumP,SCM_fixnump)
DefMonadicPred(SCM_SymbolP,SCM_symbolp)
DefMonadicPred(SCM_StringP,SCM_stringp)
DefMonadicPred(SCM_CharP,SCM_charp)
DefMonadicPred(SCM_EofP,SCM_eofp)

SCM SCM_ProcedureP (SCM x)
{ switch (SCM_tag_of(x)) {
 case SCM_PRIMITIVE_TAG:
 case SCM_CLOSURE_TAG:
 case SCM_STACK_SLICE_TAG:
 case SCM_CONTINUATION_TAG:
  return SCM_true ;
 default:
  return SCM_false ;
}
}

#define DefDyadicPred(name,operator)	\
SCM name (SCM x, SCM y)	\
{ return (SCM_2bool(operator(x,y))) ;	\
}

DefDyadicPred(SCM_EqP,SCM_eqp)

/***********************************************************************
 * Global functions on lists.
 * self corresponds the enclosed environment which is useles here.	 */

SCM SCM_Cons (SCM x, SCM y)
{ return (SCM_cons(x,y)) ;
}

SCM SCM_Car (SCM x)
{ if ( SCM_pairp(x) ) { return (SCM_car(x)) ; } ;
  SCM_error(60) ; /*TEMP*/
}

SCM SCM_Cdr (SCM x)
{ if ( SCM_pairp(x) ) { return (SCM_cdr(x)) ; } ;
  SCM_error(61) ; /*TEMP*/
}

SCM SCM_Set_Car (SCM x, SCM y)
{ if ( SCM_pairp(x) ) 
    if ( SCM_staticp(x) ) SCM_error(64) ;
    else return (SCM_set_car(x,y)) ; 
  else SCM_error(62) ; /*TEMP*/
}

SCM SCM_Set_Cdr (SCM x, SCM y)
{ if ( SCM_pairp(x) ) 
    if ( SCM_staticp(x) ) SCM_error(65) ;
    else return (SCM_set_cdr(x,y)) ; 
  SCM_error(63) ; /*TEMP*/
}

SCM SCM_Not (SCM x)
{ if ( SCM_falsep(x) ) return SCM_true ;
  else return SCM_false ;
}

/***********************************************************************
 * Arithmetic functions							*/

#define DefArithOp(name,op,err)						\
SCM name (SCM x, SCM y)							\
{ if ( SCM_fixnump(x) && SCM_fixnump(y) ) 				\
    return SCM_make_fixnum( SCM_fixnum2int(x) op SCM_fixnum2int(y)) ;	\
  else SCM_error(err) ;							\
}

DefArithOp(SCM_Plus,+,50)
DefArithOp(SCM_Minus,-,51)
DefArithOp(SCM_Times,*,52)
DefArithOp(SCM_Quotient,/,53)
DefArithOp(SCM_Remainder,%,54)

#define DefArithPred(name,op,err)					\
SCM name (SCM x, SCM y)							\
{ if ( SCM_fixnump(x) && SCM_fixnump(y) ) 				\
    return SCM_2bool(SCM_fixnum2int(x) op SCM_fixnum2int(y)) ;		\
  else SCM_error(err) ;							\
}

DefArithPred(SCM_GtP,>,55)
DefArithPred(SCM_LtP,<,56)
DefArithPred(SCM_EqnP,==,57)
DefArithPred(SCM_GeP,>=,58)
DefArithPred(SCM_LeP,<=,59)

/***********************************************************************
 * Functions on floatnums						*/

#define DefFloatOp(name,op,err)						\
SCM name (SCM x, SCM y)							\
{ if ( SCM_floatnump(x) && SCM_floatnump(y) ) 				\
    return SCM_make_floatnum(SCM_floatnum2float(x) 			\
                             op SCM_floatnum2float(y)) ; 		\
  else SCM_error(err) ;							\
}

DefFloatOp(SCM_Float_Plus,+,40)
DefFloatOp(SCM_Float_Minus,-,41)
DefFloatOp(SCM_Float_Times,*,42)
DefFloatOp(SCM_Float_Divide,/,43)

#define DefFloatPred(name,op,err)					\
SCM name (SCM x, SCM y)							\
{ if ( SCM_floatnump(x) && SCM_floatnump(y) ) 				\
    return SCM_2bool(SCM_floatnum2float(x) 				\
                     op SCM_floatnum2float(y)) ; 			\
  else SCM_error(err) ;							\
}

DefFloatPred(SCM_Float_GtP,>,44)
DefFloatPred(SCM_Float_LtP,<,45)
DefFloatPred(SCM_Float_EqnP,==,46)
DefFloatPred(SCM_Float_GeP,>=,47)
DefFloatPred(SCM_Float_LeP,<=,48)

/***********************************************************************
 * A very primitive printing mechanism. It nevertheless knows how to
 * print all types of entities. This is useful for debugging.
 * To avoid printing circular structures, SCM_show can only print a
 * limited amount of objects specified in the following global variable.*/

static int SCM_show_limit = 0 ;

SCM SCM_show (SCM x, FILE *stream) {
  if (SCM_show_limit-- < 0) 
    { fputs(" &&&",stream) ; }
  else
    switch (SCM_tag_of(x)) {
    case SCM_PAIR_TAG: {
      fputs("(",stream) ;
      { SCM object = x ;
        SCM cdrobject ;
        while (SCM_pairp(object))
          { SCM_show(SCM_car(object),stream) ;
            cdrobject = SCM_cdr(object) ;
            if (SCM_pairp(cdrobject)) { fputs(" ",stream) ;
                                        object = cdrobject ; }
            else if SCM_nullp(cdrobject) break ;
            else { fputs(" . ",stream) ;
                   SCM_show(cdrobject,stream) ; 
                   break ;
                 } ;
          } ;
        fprintf(stream,")") ; 
        break ; } ; } ;
    case SCM_SYMBOL_TAG: {
      { SCM string = SCM_symbol2string(x) ;
        char *Cstring ;
        int length,n ;
        Cstring = SCM_string2Cstring(string) ;
        length = SCM_string2size(string) ;
        while ( length>0 )
          { n = fwrite(Cstring,sizeof(char),length,stream) ;
            if ( n==0 ) SCM_error(71) ;
            length = length-n ; 
            Cstring = Cstring+n ; } ;
        break ; } ; } ;
    case SCM_EMPTY_TAG: {
      fputs("()",stream) ;
      break ; } ;
    case SCM_CLOSURE_TAG: {
      fprintf(stream,"#<Closure:x%x>",(void*)(x->closure).behavior) ;
      break ; } ;
    case SCM_PRIMITIVE_TAG: {
      fputs("#<Primitive:",stream) ;
      SCM_show(SCM_primitive2name(x),stream) ;
      fputs(">",stream) ;
      break ; } ;
    case SCM_FIXNUM_TAG: {
      fprintf(stream,"%d",(x->fixnum).value) ;
      break ; } ;
    case SCM_FLOATNUM_TAG: {
      fprintf(stream,"%g",(x->floatnum).value) ;
      break ; } ;
    case SCM_UNDETERMINED_TAG: {
      fputs("#<UFO>",stream) ;  /* or raise an error ? */
      break ; } ;
    case SCM_TRUE_TAG: {
      fputs("#T",stream) ;
      break ; } ;
    case SCM_FALSE_TAG: {
      fputs("#F",stream) ;
      break ; } ;
    case SCM_EOF_TAG: {
      fputs("#<EndOfFile>",stream) ;
      break ; } ;
    case SCM_BOX_TAG: {         /* Should be invisible ? */
      fprintf(stream,"#<Box:x%x>",(void*)(x)) ;
      break ; } ;
    case SCM_STRING_TAG: {
      fputc('"',stream) ;
      { int length = SCM_string2size(x) ;
        char *Cstring = SCM_string2Cstring(x) ;
        int n ;
        while ( length>0 )
          { n = fwrite(Cstring,sizeof(char),length,stream) ;
            if ( n==0 ) SCM_error(72) ;
            length = length-n ;
            Cstring = Cstring+n ; } ;
        fputc('"',stream) ;
        break ; } ; } ;
    case SCM_CHAR_TAG: {
      fputc((x->character).C_char,stream) ;
      break ; } ;
    case SCM_PORT_TAG: {
      if (SCM_eqp(x,SCM_stdin))
        { fputs("#<stdin>",stream) ; break ; } ;
      if (SCM_eqp(x,SCM_stdout))
        { fputs("#<stdout>",stream) ; break ; } ;
      if (SCM_eqp(x,SCM_stderr))
        { fputs("#<stderr>",stream) ; break ; } ;
      fputs("#<Port>",stream) ; /* Indicate the port number */
      break ; } ;
    case SCM_STACK_SLICE_TAG: {
      fprintf(stream,"#<Stack_Slice:x%x>",(void*)(x)) ; 
      break ; } ;
    case SCM_CONTINUATION_TAG: {
      fprintf(stream,"#<Continuation:x%x>",(void*)(x)) ;
      break ; } ;
    case SCM_PROCESS_TAG: {
      fprintf(stream,"#<Process:x%x>",(void*)(x)) ;
      break ; } ;
    case SCM_MODULE_TAG: {
      fprintf(stream,"#<Module:x%x>",(void*)(x)) ;
      break ; } ;
    case SCM_ROOT_TAG: {
      fprintf(stream,"#<Roots:x%x>",(void*)(x)) ;
      break ; } ;
    case SCM_FRAME_TAG: {
      SIZE_T i, size = x->frame.size ;
      SCM next_frame = x->frame.next ;
      fprintf(stream,"#<Frame:x%x[",(void*)(x)) ;
      for (i=0; i<size; i++) {
        fprintf(stream,"(%d)=",i) ;
        SCM_show(x->frame.value[i],stream) ;
        fprintf(stream,", ") ;
      }
      fprintf(stream,"] Next=") ;
      if ( !(SCM_eqp(next_frame,SCM_The_Null_Environment)) ) {
        SCM_show(next_frame,stream) ;
      }
      fprintf(stream,">") ;
      break ; }
    case SCM_FUNCTION_TAG: {
      fprintf(stream,"#<Function:x%x>",(void*)(x)) ;
      break ; } ;
    default: 
      SCM_error(73) ;
    } 
}

SCM SCM_Basic_Write (SCM x) {
  SCM_show_limit = 100 ;  /* Shows a maximal of 100 objects	*/
  SCM_show(x,stdout) ;
  fflush(stdout) ;
  return (SCM_true) ;
}

/* Safe conversion of a Scheme fixnum into a Scheme character. 		*/
SCM SCM_Integer2Char (SCM x) {
  if (SCM_fixnump(x))
    { int v = SCM_fixnum2int(x) ;
      return ((SCM) &SCM_characters[v]) ; }
  else SCM_error(31) ;
}

/* Safe conversion of a Scheme character into a Scheme fixnum.		*/
SCM SCM_Char2Integer (SCM x) {
  if (SCM_charp(x)) return (SCM_make_fixnum(SCM_char2int(x))) ;
  else SCM_error(32) ;
}

/* Safe conversion of a Scheme symbol to a Scheme string.		*/
SCM SCM_Symbol2String (SCM x) {
  if (SCM_symbolp(x)) return (SCM_symbol2string(x)) ;
  else SCM_error(33) ;
}

/* This function creates new symbols but do not ensure uniqueness.	*/
SCM SCM_Make_Symbol (SCM x) {
  if (SCM_stringp(x)) return (SCM_make_symbol(x)) ;
  else SCM_error(34) ;
}

/* Read a character from a stream. Use the last poken char if any.	*/
SCM SCM_Read_Char (SCM x) {
  if (SCM_streamp(x))
    { SCM result = SCM_port2poken_char(x) ;
      if ( result == (SCM) NULL )
        { char c ;
          c = fgetc(SCM_port2stream(x)) ;
          if ( c==EOF ) return SCM_eof ;
          else return SCM_int2char((int) c) ; }
      else { SCM_port2poken_char(x) = (SCM) NULL ;
             return result ; } ; }
  else SCM_error(80) ;
}

/* Peek the next char (if not already done).				*/
SCM SCM_Peek_Char (SCM x) {
  if (SCM_streamp(x)) 
    { SCM result = SCM_port2poken_char(x) ;
      if ( result == (SCM) NULL )
        { char c ;
          c = fgetc(SCM_port2stream(x)) ;
          result = ( c==EOF ) ? SCM_eof : SCM_int2char((int) c) ; } ;
      SCM_port2poken_char(x) = result ;
      return result ; }
  else SCM_error(81) ;
}

/* Return the length of a string (or rope).				*/
SCM SCM_String_Length (SCM x) {
  if (SCM_stringp(x)) return SCM_make_fixnum(SCM_string2size(x)) ;
  else SCM_error(90) ;
}

/* Return the Yth char of a string (or rope). Check the bounds.		*/
SCM SCM_String_Ref (SCM x, SCM y) 
{ if (SCM_stringp(x))
    { if (SCM_fixnump(y))
        { int index = SCM_fixnum2int(y) ;
          if ( ( 0<=index ) & (index<SCM_string2size(x)) ) 
            return (SCM_int2char((int) SCM_string2Cstring(x)[index])) ;
          else SCM_error(91) ; }
      else SCM_error(92) ; }
  else SCM_error(93) ;
}

/* Modifies the Yth character of a string				*/
SCM SCM_String_Set (SCM x, SCM y, SCM z) 
{ if (SCM_stringp(x)) 
    { if (SCM_mutable_stringp(x))
        { if (SCM_fixnump(y))
            { int index = SCM_fixnum2int(y) ;
              if ( ( 0<=index ) & (index<SCM_string2size(x)) )
                { if (SCM_charp(z))
                    { char c = SCM_string2Cstring(x)[index] ;
                      SCM_string2Cstring(x)[index] = (char) SCM_char2int(z) ;
                      return (SCM_int2char((int) c)) ; }
                  else SCM_error(94) ; }
                else SCM_error(95) ; }
          else SCM_error(96) ; }
      else SCM_error(97) ; }
  else SCM_error(98) ;
}

/***********************************************************************
 * A primitive error mechanism.
 * Just report the number of the error and abort.			*/

SCM SCM_error (unsigned int code) {
  fflush(stdout) ; /* Flush normal output */
  SCM_report_usage() ;
  fprintf(stderr,"Abort on error %d.\n",code) ; fflush(stderr) ;
  exit(code) ;
}

/* Produce a report and abort.						*/
SCM SCM_report_error(unsigned int code, char *file, unsigned int line) {
  fflush(stdout) ; /* Flush normal output */
  fprintf(stderr,"Error %u occurred in file %s at line %u.\n",code,file,line) ;
  SCM_error(code) ;
}

/* Terminates the computation and exit with a precise value.		*/
SCM SCM_Exit (SCM x) {
  if (SCM_fixnump(x)) 
    { SCM_report_usage() ;
      exit((unsigned int) SCM_fixnum2int(x)) ; }
  else SCM_error(20) ;
}

/***********************************************************************
  Some static constants 						*/

#define DefineImmediateObject(name,tag)                     \
struct SCM_immediate_object SCM_CONC(name,_object) =        \
  {{tag, (SCM) NULL}} ;                                     \
SCM name = (SCM) &(SCM_CONC(name,_object)) ;

DefineImmediateObject(SCM_true,  SCM_TRUE_TAG) ;
SCM SCM_T = (SCM) &(SCM_true_object) ;
DefineImmediateObject(SCM_false, SCM_FALSE_TAG) ;
SCM SCM_F = (SCM) &(SCM_false_object) ;
DefineImmediateObject(SCM_empty, SCM_EMPTY_TAG) ;
SCM SCM_NIL = (SCM) &(SCM_empty_object) ;
DefineImmediateObject(SCM_undef, SCM_UNDETERMINED_TAG) ;
DefineImmediateObject(SCM_eof,   SCM_EOF_TAG) ;

#define DefinePort(name,stream,way)                       \
static struct SCM_port SCM_CONC(name,_object) =           \
  {{SCM_PORT_TAG, (SCM) NULL}, way, (SCM) NULL, stream} ; \
SCM name = (SCM) &(SCM_CONC(name,_object)) ;

DefinePort(SCM_stdin,  stdin, O_RDONLY) ;
SCM SCM_STDIN = (SCM) &(SCM_stdin_object) ;
DefinePort(SCM_stdout, stdout,O_WRONLY) ;
SCM SCM_STDOUT = (SCM) &(SCM_stdout_object) ;
DefinePort(SCM_stderr, stderr,O_WRONLY) ;
SCM SCM_STDERR = (SCM) &(SCM_stderr_object) ;

/* the options of the command gathered in a list. 
 * Will be initialized by SCM_initialize() 				 */
SCM SCM_options ;

/* The approximate bottom of the C_stack. Hope that nothing important
 * change under this line. The second variable says if the stack grows
 * downward (as normal in Un*x) or up. This parameter is automatically
 * set, if necessary, in SCM_initialize.				*/
void *SCM_Cstack_bottom = (void*) NULL ;
SCM_stack_growing_type SCM_stack_grows = DOWN ;

/***********************************************************************
 * Some static global variables predefined to hold SCM values           */

#define DefineGlobalFunction(name,C_function,arity,C_string)      \
static struct SCM_string SCM_CONC(name,_string) =                 \
  {{SCM_STRING_TAG, (SCM) NULL}, SIZEOF(C_string)-1, C_string} ;  \
static struct SCM_primitive SCM_CONC(name,_object) =              \
  {{SCM_PRIMITIVE_TAG, (SCM) NULL},                               \
     C_function, arity, (SCM) &(SCM_CONC(name,_string)) } ;	  \
SCM SCM_CONC(SCM_,name) = (SCM) &(SCM_CONC(name,_object)) ;

DefineGlobalFunction(PAIRP,SCM_PairP,1,"pair?") ;
DefineGlobalFunction(NULLP,SCM_NullP,1,"null?") ;
DefineGlobalFunction(INTEGERP,SCM_FixnumP,1,"fixnum?") ;
DefineGlobalFunction(SYMBOLP,SCM_SymbolP,1,"symbol?") ;
DefineGlobalFunction(STRINGP,SCM_StringP,1,"string?") ;
DefineGlobalFunction(CHARP,SCM_CharP,1,"char?") ;
DefineGlobalFunction(ENDOFFILEP,SCM_EofP,1,"eof-object?") ;
DefineGlobalFunction(PROCEDUREP,SCM_ProcedureP,1,"procedure?") ;
DefineGlobalFunction(EQP,SCM_EqP,2,"eq?") ;
DefineGlobalFunction(CONS,SCM_Cons,2,"cons") ;
DefineGlobalFunction(CAR,SCM_Car,1,"car") ;
DefineGlobalFunction(CDR,SCM_Cdr,1,"cdr") ;
DefineGlobalFunction(SETCAR,SCM_Set_Car,2,"set-car!") ;
DefineGlobalFunction(SETCDR,SCM_Set_Cdr,2,"set-cdr!") ;
DefineGlobalFunction(PLUS,SCM_Plus,2,"+") ;
DefineGlobalFunction(MINUS,SCM_Minus,2,"-") ;
DefineGlobalFunction(TIMES,SCM_Times,2,"*") ;
DefineGlobalFunction(QUOTIENT,SCM_Quotient,2,"quotient") ;
DefineGlobalFunction(REMAINDER,SCM_Remainder,2,"remainder") ;
DefineGlobalFunction(GTP,SCM_GtP,2,">") ;
DefineGlobalFunction(LTP,SCM_LtP,2,"<") ;
DefineGlobalFunction(GEP,SCM_GeP,2,">=") ;
DefineGlobalFunction(LEP,SCM_LeP,2,"<=") ;
DefineGlobalFunction(EQNP,SCM_EqnP,2,"=") ;
DefineGlobalFunction(BASICWRITE,SCM_Basic_Write,1,"basicwrite") ;
DefineGlobalFunction(INT2CHAR,SCM_Integer2Char,1,"integer->char") ;
DefineGlobalFunction(CHAR2INT,SCM_Char2Integer,1,"char->integer") ;
DefineGlobalFunction(SYMBOL2STRING,
         SCM_Symbol2String,1,"symbol->string") ;
DefineGlobalFunction(MAKE_SYMBOL,SCM_Make_Symbol,1,"make-symbol") ;
DefineGlobalFunction(EXIT,SCM_Exit,1,"exit") ;
DefineGlobalFunction(GETUNIVERSALTIME,
         SCM_Get_Universal_Time,0,"get-universal-time" ) ;
DefineGlobalFunction(FLOAT_PLUS,SCM_Float_Plus,2,"float:+") ;
DefineGlobalFunction(FLOAT_MINUS,SCM_Float_Minus,2,"float:-") ;
DefineGlobalFunction(FLOAT_TIMES,SCM_Float_Times,2,"float:*") ;
DefineGlobalFunction(FLOAT_DIVIDE,SCM_Float_Divide,2,"float:/") ;
DefineGlobalFunction(FLOAT_GTP,SCM_Float_GtP,2,"float:>") ;
DefineGlobalFunction(FLOAT_LTP,SCM_Float_LtP,2,"float:<") ;
DefineGlobalFunction(FLOAT_GEP,SCM_Float_GeP,2,"float:>=") ;
DefineGlobalFunction(FLOAT_LEP,SCM_Float_LeP,2,"float:<=") ;
DefineGlobalFunction(FLOAT_EQNP,SCM_Float_EqnP,2,"float:=") ;
DefineGlobalFunction(READ_CHAR,SCM_Read_Char,1,"read-char") ;
DefineGlobalFunction(PEEK_CHAR,SCM_Peek_Char,1,"peek-char") ;
DefineGlobalFunction(STRING_LENGTH,
         SCM_String_Length,1,"string-length") ;
DefineGlobalFunction(STRING_REF,SCM_String_Ref,2,"string-ref") ;
DefineGlobalFunction(STRING_SET,SCM_String_Set,3,"string-set!") ;
DefineGlobalFunction(NOT,SCM_Not,1,"not") ;

/***********************************************************************
 * All these global variables have to be initialized by explicit 
 * allocations. These are gathered in this routine. It takes the
 * command arguments to convert them into a list of Scheme strings.
 *						                      */

void SCM_initialize (int argc, char *argv[])
{ int i ;
  /* Check that the C stack bottom is already set. 
     It must be set just after main() if possible. */
  if ( SCM_Cstack_bottom == ((void*) NULL) )
    { fprintf(stderr,"Initialization error: SCM_Cstack_bottom not set.\n") ;
      SCM_error(1) ; } ;
  /* Determine the way the stack grows. */
  if ( SCM_Cstack_bottom < ((void*) &i) ) SCM_stack_grows = UP ;
  /* Initialize the allocator(s) */
  initialize_allocator() ;
  /* Initialize characters		*/
  for (i=0 ; i<SCM_CHAR_SIZE ; i++) 
    { SCM_characters[i].C_char = i ;
      SCM_characters[i].header.tag = SCM_CHAR_TAG ; } ;
  /* Initialize the first natural numbers	*/
  for (i=0 ; i<SCM_SMALL_FIXNUM_LIMIT ; i++)
    { SCM_small_fixnums[i].value = i ;
      SCM_small_fixnums[i].header.tag = SCM_FIXNUM_TAG ; } ;
  staticize_allocator() ; 
  /* Handle command options: turn them into a mutable? list. */
  SCM_options = SCM_empty ;
  for ( i=argc-1 ; i>=0 ; i--)
    { SCM string = SCM_make_string(argv[i]) ;
      SCM_options = SCM_cons(string,SCM_options) ;
    } ;
}

#ifdef GC_STOP_AND_COPY
/***********************************************************************
 * Debug utilities. 							*/

void SCM_dump ()
{ char *index ;
  SIZE_T i ;
  int show_limit = 2 ; /* To limit the printed hints */
  char fmt[] = "\n Dumping the %ser space from 0x%x to 0x%x.\n" ;
  if ( way==LOW2HIGH )
    { index = (char*) zone_static_end ;
      fprintf(stderr,fmt,"low",index,zone_index) ; }
  else { index = (char*) zone_middle ;
         fprintf(stderr,fmt,"high",index,zone_index) ; } ;
  while ( index < zone_index )
    { fprintf(stderr,"0x%x :\t",index) ;
      { SCM o = (SCM) index ;
        SCM_show_limit = show_limit ;
        switch(SCM_tag_of(o)) {
          case SCM_PAIR_TAG: {
            fprintf(stderr,"PAIR\tcar=xg191%x, cdr=0x%x\n\tHint: ",
                    SCM_car(o), SCM_cdr(o)) ;
            SCM_show(o,stderr) ; 
            break ; } ;
          case SCM_SYMBOL_TAG: {
            fprintf(stderr,"SYMBOL\tstring=0x%x Hint: ",SCM_symbol2string(o)) ;
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_EMPTY_TAG:
          case SCM_UNDETERMINED_TAG: 
          case SCM_TRUE_TAG:
          case SCM_FALSE_TAG:
          case SCM_EOF_TAG: {
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_CLOSURE_TAG: {
            SIZE_T size = (o->closure).size ;
            fprintf(stderr,"CLOSURE\tbehavior=0x%x, arity=%u, size=%u",
                    (o->closure).behavior, (o->closure).arity, size ) ;
            for ( i=0 ; i<size ; i++ )
              { fprintf(stderr,"\n\tClosed value[%u] = 0x%x Hint: ",
                        i, SCM_closed_value(o,i) ) ;
                SCM_show_limit = show_limit ;
                SCM_show(SCM_closed_value(o,i),stderr) ; } ;
            break ; } ;
          case SCM_PRIMITIVE_TAG: {
            fprintf(stderr,"PRIMITIVE behavior=0x%x, arity=%u name=",
                    (o->closure).behavior, (o->closure).arity ) ;
            SCM_show(SCM_primitive2name(o),stderr) ;
            break ; } ;
          case SCM_FIXNUM_TAG: {
            fprintf(stderr,"FIXNUM\t%d", (o->fixnum).value) ;
            break ; } ;
          case SCM_FLOATNUM_TAG: {
            fprintf(stderr,"FLOATNUM\t%g", (o->floatnum).value) ;
            break ; } ;
          case SCM_BOX_TAG: {
            fprintf(stderr,"BOX\t0x%x Hint: ",(void*) SCM_box_content(o)) ;
            SCM_show(SCM_box_content(o),stderr) ;
            break ; } ;
          case SCM_STRING_TAG: {
            fprintf(stderr,"STRING\t") ;
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_CHAR_TAG: {
            fprintf(stderr,"CHAR\t") ;
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_PORT_TAG: {
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_STACK_SLICE_TAG: {
            SCM_show(o,stderr) ;
            break ; } ;
          case SCM_CONTINUATION_TAG: {
            fprintf(stderr,"CONTINUATION\ttransformer=0x%x, below=0x%x",
                    SCM_continuation2transformer(o), 
                    SCM_continuation2below(o) ) ;
            break ; } ;
          case SCM_MODULE_TAG: {
            SIZE_T size = (o->module).size ;
            fprintf(stderr,"MODULE\tsize=%u",size) ;
            for ( i=0 ; i<((size>0)?size:1) ; i++ )
              { fprintf(stderr,"\n\tGlobal[%u] = 0x%x Hint: ",
                        i, (o->module).global[i] ) ; 
                SCM_show_limit = show_limit ;
                SCM_show((o->module).global[i],stderr) ; } ;
            break ; } ;
          case SCM_ROOT_TAG: {
            SIZE_T size = (o->root).size ;
            fprintf(stderr,"ROOTS\tsize=%u",size) ;
            for ( i=0 ; i<((size>0)?size:1) ; i++ )
              { fprintf(stderr,"\n\tModule[%u] = 0x%x",
                        i, (o->root).module[i] ) ; } ;
            break ; } ;
          default:
            SCM_error(300) ;
          } ; } ;
      fputs("\n",stderr) ;
      index = index + SCM_size((SCM) index) ; } ;
  fprintf(stderr,"End of dump.\n") ;
  fflush(stderr) ;
}
#endif /* GC_STOP_AND_COPY */


/* Notes:
   faire post-mortem-dump ?
   tout separer par fonctions et mettre en librairie
   faire pointeur apres le tag.
   inclure Meroon
   verifier pur ansi ?
   realloc plutot que alloc et extension
*/

/**************************************
Coming from rtd_scheme.c (modified for the new invokation protocol)
*************/

/***********************************************************************
 * Continuations (or so :~)
 * This is not guaranteed to work but it is written in pure C.		*/

SCM SCM_Call_CC (SCM f) {
  SCM result ;
  int r, count ;
  char *i, *o ;
  void *current_stack_ptr = (void*) &result ;
  SIZE_T stack_length = abs(SCM_Cstack_bottom - current_stack_ptr) ;
  result = SCM_make_stack_slice(stack_length) ;
  (result->stack_slice).stack_slice_length = stack_length ;
  i = SCM_Cstack_bottom ;
  o = (result->stack_slice).stack_char ;
  r = setjmp(((result->stack_slice).stack_jmp_buf)) ;
  if ( r==0 ) { 
    if (SCM_stack_grows==DOWN)
      for ( count=stack_length ; count>=0 ; i--,o++,count-- ) *o=(*i) ;
    else for ( count=stack_length ; count>=0 ; i++,o++,count-- ) *o=(*i) ;
    { SCM frame = SCM_make_frame(1+1+1) ;
      frame->frame.value[0] = result ;
      return SCM_call(f,frame) ;
    }
  }
  else { SCM value = (SCM) r ;
         return value ; } ;
}
DefineGlobalFunction(CALL_CC,SCM_Call_CC,1,"call/cc") ;

#define STEP 10
void SCM_invoke_stack_slice (SCM function, SCM v1)
{ /* The following worked some time then failed!? There is only a minor
       alignment difference with the new version and it seems to be
       insensitive to the value of STEP. 
       SCM mark ;
       char *current_stack_ptr = (char*) &mark ; */
  void *current_stack_ptr ;
  current_stack_ptr = (void*) &current_stack_ptr ;
  /* This test tries to check if both the stack and frame pointers 
     will not be overwritten when copying back the stack_slice.
     This works on Sony News with cc (and gcc???). */
  if ( ((SIZE_T)abs(SCM_Cstack_bottom - current_stack_ptr)) <
      ( (function->stack_slice).stack_slice_length 
       + ((SIZE_T) 2*STEP*SIZEOF(SCM)) ) )
    { SCM room[STEP] ; /* Augment the stack */
      SCM_invoke_stack_slice(function,v1) ; }
  else 
    { char *i, *o ;
      int count ;
      static SCM save_function, save_v1 ;
      save_function = function ;
      save_v1 = v1 ;
      i = SCM_Cstack_bottom ;
      o = (function->stack_slice).stack_char ; 
      if (SCM_stack_grows==DOWN)
        { for ( count=(function->stack_slice).stack_slice_length ;
               count>=0 ; i--,o++,count-- ) *i=(*o) ; }
      else for ( count=(function->stack_slice).stack_slice_length ;
                count>=0 ; i++,o++,count-- ) *i=(*o) ;
      longjmp((save_function->stack_slice).stack_jmp_buf,save_v1) ; 
    } ; 
}

/***********************************************************************
  The generic invokers of functions.
  They use errors numbered 2xy.						*/

void SCM_first_invokation(SCM thunk) 
{ SCM_invoke0 (thunk) ;
}

SCM SCM_invoke0 (SCM function)
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 ) 
      { if ( arity==0 ) return behavior() ;
        else SCM_error(200) ; } 
    else switch (SCM_minimal_arity(arity)) {
    case 0:
      return behavior(0,((SCM*) NULL)) ;
    default:
      SCM_error(201) ; } ;
  } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 ) 
      { if ( arity==1 ) return behavior(function) ;
        else SCM_error(202) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 1:
      return behavior(function,0,((SCM*) NULL)) ; 
    default: SCM_error(203) ; } ;
  } ;
 case SCM_STACK_SLICE_TAG:
  SCM_error(204) ; 
 default: SCM_error(205) ;
}
}

SCM SCM_invoke1 (SCM function, SCM v1)
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 ) 
      { if ( arity==1 ) return behavior(v1) ;
        else SCM_error(210) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 0:
      { SCM arguments[1] ;
        arguments[0]= v1 ;
        return behavior(1,arguments) ; }
    case 1:
      return behavior(v1,0,((SCM*) NULL)) ; 
    default:
      SCM_error(211) ; } ;
  } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 ) 
      { if ( arity==2 ) return behavior(function,v1) ;
        else SCM_error(212) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 1:
      { SCM arguments[1] ;
        arguments[0]= v1 ;
        return behavior(function,1,arguments) ; }
    case 2:
      return behavior(function,v1,0,((SCM*) NULL)) ; 
    default:
      SCM_error(213) ; } ;
  } ;
 case SCM_STACK_SLICE_TAG:
  SCM_invoke_stack_slice(function,v1) ;
 default: SCM_error(214) ;
}
}

SCM SCM_invoke2 (SCM function, SCM v1, SCM v2)
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 ) 
      { if ( arity==2 ) return behavior(v1,v2) ;
        else SCM_error(220) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 0:
      { SCM arguments[2] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        return behavior(2,arguments) ; }
    case 1:
      { SCM arguments[1] ;
        arguments[0]= v2 ;
        return behavior(v1,1,arguments) ; }
    case 2:
      return behavior(v1,v2,0,((SCM*) NULL)) ; 
    default:
      SCM_error(221) ; } ;
  } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 ) 
      { if ( arity==3 ) return behavior(function,v1,v2) ;
        else SCM_error(222) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 1:
      { SCM arguments[2] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        return behavior(function,2,arguments) ; }
    case 2:
      { SCM arguments[1] ;
        arguments[0]= v2 ;
        return behavior(function,v1,1,arguments) ; }
    case 3:
      return behavior(function,v1,v2,0,((SCM*) NULL)) ; 
    default:
      SCM_error(223) ; } ;
  } ;
 case SCM_STACK_SLICE_TAG:
  SCM_error(224) ; 
 default: SCM_error(225) ;
}
}

SCM SCM_invoke3 (SCM function, SCM v1, SCM v2, SCM v3)
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 ) 
      { if ( arity==3 ) return behavior(v1,v2,v3) ;
        else SCM_error(230) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 0:
      { SCM arguments[3] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        arguments[2]= v3 ;
        return behavior(3,arguments) ; }
    case 1:
      { SCM arguments[2] ;
        arguments[0]= v2 ;
        arguments[1]= v3 ;
        return behavior(v1,2,arguments) ; }
    case 2:
      { SCM arguments[1] ;
        arguments[0]= v3 ;
        return behavior(v1,v2,1,arguments) ; }
    case 3:
      return behavior(v1,v2,v3,0,((SCM*) NULL)) ; 
    default:
      SCM_error(231) ; } ;
  } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 ) 
      { if ( arity==4 ) return behavior(function,v1,v2,v3) ;
        else SCM_error(232) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 1:
      { SCM arguments[3] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        arguments[2]= v3 ;
        return behavior(function,3,arguments) ; }
    case 2:
      { SCM arguments[2] ;
        arguments[0]= v2 ;
        arguments[1]= v3 ;
        return behavior(function,v1,2,arguments) ; }
    case 3:
      { SCM arguments[1] ;
        arguments[0]= v3 ;
        return behavior(function,v1,v2,1,arguments) ; }
    case 4:
      return behavior(function,v1,v2,v3,0,((SCM*) NULL)) ; 
    default:
      SCM_error(233) ; } ;
  } ;
 case SCM_STACK_SLICE_TAG:
  SCM_error(234) ;
 default: SCM_error(235) ; 
}
}

SCM SCM_invoke4 (SCM function, SCM v1, SCM v2, SCM v3, SCM v4)
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 ) 
      { if ( arity==4 ) return behavior(v1,v2,v3,v4) ;
        else SCM_error(240) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 0:
      { SCM arguments[4] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        arguments[2]= v3 ;
        arguments[3]= v4 ;
        return behavior(4,arguments) ; }
    case 1:
      { SCM arguments[3] ;
        arguments[0]= v2 ;
        arguments[1]= v3 ;
        arguments[2]= v4 ;
        return behavior(v1,3,arguments) ; }
    case 2:
      { SCM arguments[2] ;
        arguments[0]= v3 ;
        arguments[1]= v4 ;
        return behavior(v1,v2,2,arguments) ; }
    case 3:
      { SCM arguments[1] ;
        arguments[0]= v4 ;
        return behavior(v1,v2,v3,1,arguments) ; }
    case 4:
      return behavior(v1,v2,v3,v4,0,((SCM*) NULL)) ; 
    default:
      SCM_error(241) ; } ;
  } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 ) 
      { if ( arity==5 ) return behavior(function,v1,v2,v3,v4) ;
        else SCM_error(242) ; }
    else switch (SCM_minimal_arity(arity)) {
    case 1:
      { SCM arguments[4] ;
        arguments[0]= v1 ;
        arguments[1]= v2 ;
        arguments[2]= v3 ;
        arguments[3]= v4 ;
        return behavior(function,4,arguments) ; }
    case 2:
      { SCM arguments[3] ;
        arguments[0]= v2 ;
        arguments[1]= v3 ;
        arguments[2]= v4 ;
        return behavior(function,v1,3,arguments) ; }
    case 3:
      { SCM arguments[2] ;
        arguments[0]= v3 ;
        arguments[1]= v4 ;
        return behavior(function,v1,v2,2,arguments) ; }
    case 4:
      { SCM arguments[1] ;
        arguments[0]= v4 ;
        return behavior(function,v1,v2,v3,1,arguments) ; }
    case 5:
      return behavior(function,v1,v2,v3,v4,0,((SCM*) NULL)) ; 
    default:
      SCM_error(243) ; } ;
  } ;
 case SCM_STACK_SLICE_TAG:
  SCM_error(244) ;
 default: SCM_error(245) ; 
}
}

/* The polyadic invoker							*/
SCM SCM_invokeN (SCM function, int number, SCM args[])
{ switch (SCM_tag_of(function)) {
 case SCM_PRIMITIVE_TAG:
  { SCM (*behavior)() = (function->primitive).behavior ;
    int arity = (function->primitive).arity ;
    if ( arity>=0 )
      { if ( arity == number )
          switch (arity) {
          case 0:
            return behavior() ;
          case 1:
            return behavior(args[0]) ;
          case 2:
            return behavior(args[0],args[1]) ;
          case 3:
            return behavior(args[0],args[1],args[2]) ;
          case 4:
            return behavior(args[0],args[1],args[2],args[3]);
          case 5:
            return behavior(args[0],args[1],args[2],args[3],args[4]);
          case 6:
            return behavior(args[0],args[1],args[2],args[3],args[4],args[5]);
          default:
            return behavior(args) ;
          } else SCM_error(251) ; }
    else 
      { int min_arity = SCM_minimal_arity(arity) ;
        if ( number >= min_arity )
          switch (min_arity) {
          case 0:
            return behavior(number,args) ;
          case 1:
            return behavior(args[0],number-1,args+1) ;
          case 2:
            return behavior(args[0],args[1],number-2,args+2) ;
          case 3:
            return behavior(args[0],args[1],args[2],number-3,args+3) ;
          case 4:
            return behavior(args[0],args[1],args[2],args[3],number-4,args+4) ;
          case 5:
            return behavior(args[0],args[1],args[2],args[3],
                            args[4],number-5,args+5) ;
          case 6:
            return behavior(args[0],args[1],args[2],args[3],
                            args[4],args[5],number-6,args+6) ;
          default:
            return behavior(number,args) ;
          } else SCM_error(252) ; } ; } ;
 case SCM_CLOSURE_TAG: 
  { SCM (*behavior)() = (function->closure).behavior ;
    int arity = (function->closure).arity ;
    if ( arity>=0 )
      { if ( (1+number) == arity )
          switch (arity) {
          case 1:
            return behavior(function) ;
          case 2:
            return behavior(function,args[0]) ;
          case 3:
            return behavior(function,args[0],args[1]) ;
          case 4:
            return behavior(function,args[0],args[1],args[2]) ;
          case 5:
            return behavior(function,args[0],args[1],args[2],args[3]);
          case 6:
            return behavior(function,args[0],args[1],args[2],args[3],args[4]);
          default:
            return behavior(function,args) ;
          } else SCM_error(254) ; }
    else 
      { int min_arity = SCM_minimal_arity(arity) ;
        if ( (1+number) >= min_arity )
          switch (min_arity) {
          case 1:
            return behavior(function,number,args) ;
          case 2:
            return behavior(function,args[0],number-1,args+1) ;
          case 3:
            return behavior(function,args[0],args[1],number-2,args+2) ;
          case 4:
            return behavior(function,args[0],args[1],args[2],number-3,args+3) ;
          case 5:
            return behavior(function,args[0],args[1],args[2],args[3]
                            ,number-4,args+4) ;
          default:
            return behavior(function,number,args) ;
          } else SCM_error(255) ; } ; } ;
 case SCM_STACK_SLICE_TAG:
  { if ( number==1 ) SCM_invoke_stack_slice(function,args[0]) ;
    /* error code is 257 since only the eigth low bits are significant */
    else SCM_error(257) ; } 
 default: SCM_error(258) ;  
}
}

/************************************************************************
 * These must be there for compatibility with rtq_scheme.[ch]		*/

SCM SCM_null_process ;


/*************************************
Things added for chap6f *************/

DefineImmediateObject(SCM_nullenv, SCM_NULLENV_TAG) ;
SCM SCM_The_Null_Environment = (SCM) &(SCM_nullenv_object) ;
SCM SCM_The_Environment = (SCM) &(SCM_nullenv_object) ;

SCM SCM_call (SCM fun, SCM frame) {
  switch (SCM_tag_of(fun)) {
  case SCM_FUNCTION_TAG: {
    SCM (*behavior)() = (fun->function).behavior ;
    SCM closed_environment = (fun->function).environment ;
    return behavior(frame,closed_environment) ;
  }
  case SCM_PRIMITIVE_TAG: {
    SCM (*behavior)() = (fun->primitive).behavior ;
    int arity = (fun->primitive).arity ;
    if ( arity==frame->frame.size-2 ) {
      switch ( arity ) {
      case 0: { return behavior() ;
              }
      case 1: { SCM v1 = frame->frame.value[0] ;
                return behavior(v1) ;
              }
      case 2: { SCM v1 = frame->frame.value[0] ;
                SCM v2 = frame->frame.value[1] ;
                return behavior(v1,v2) ;
              }
      case 3: { SCM v1 = frame->frame.value[0] ;
                SCM v2 = frame->frame.value[1] ;
                SCM v3 = frame->frame.value[2] ;
                return behavior(v1,v2,v3) ;
              }
      default: { SCM_error(221) ;
               }
      }
    }
    else SCM_error(220) ;
  }
  case SCM_STACK_SLICE_TAG: {
    if ( 1==frame->frame.size-2 ) {
      SCM v1 = frame->frame.value[0] ;
      SCM_invoke_stack_slice(fun,v1) ;
    }
  else SCM_error(222) ;
  }
  default: { SCM_error(205) ;
           }
  }
}

/* List is a polyadic function which receives its arguments in a C vector.
 * The protocol implemented in SCM_invoke* will call it with the value
 * of LIST as first argument.						*/
SCM SCM_List (SCM args, SCM nullenv)
{ SCM result = SCM_empty ;
  int i = args->frame.size-3 ;
  for ( i ; i>=0 ; i-- )
    { result = SCM_cons(args->frame.value[i],result) ; } ;
  return result ;
}
static struct SCM_primitive SCM_LIST_object =
  {{SCM_FUNCTION_TAG, (SCM) NULL}, SCM_List };
SCM SCM_LIST = (SCM) &(SCM_LIST_object) ;

SCM SCM_The_Dynamic_Environment = (SCM) &(SCM_nullenv_object) ;

struct SCM_frame SCM_nullframe_object =
  {{SCM_FRAME_TAG, (SCM) NULL}, (SCM) NULL, (SIZE_T) 0, (SCM) NULL} ;  
SCM SCM_The_Null_Frame = (SCM) &(SCM_nullframe_object) ;

SCM SCM_Bind_DE (SCM key, SCM value, SCM thunk) {
  SCM result ;
  struct SCM_dynamic_link dynlink ;
  dynlink.header.tag = SCM_DYNENV_TAG ;
  dynlink.next = SCM_The_Dynamic_Environment ;
  dynlink.key = key ;
  dynlink.value = value ;
  SCM_The_Dynamic_Environment = (SCM) &dynlink ;
  result = SCM_call(thunk,SCM_The_Null_Frame) ;
  SCM_The_Dynamic_Environment = dynlink.next ;
 return result ;
}
DefineGlobalFunction(BIND_DE,SCM_Bind_DE,3,"bind/de") ;

SCM SCM_Assoc_DE (SCM key, SCM success) {
  SCM dynenv ;
  SCM frame ;
  for ( dynenv=SCM_The_Dynamic_Environment ; 
       !(SCM_eqp(dynenv,SCM_The_Null_Environment)) ;
       dynenv=dynenv->dynamic_link.next ) {
    if ( (SCM_eqp(dynenv->dynamic_link.key,key)) ) {
      return dynenv->dynamic_link.value ;
    } 
  } ;
  frame = SCM_make_frame(1+1+1) ;
  frame->frame.value[0] = key ;
  return SCM_call(success,frame) ;
}
DefineGlobalFunction(ASSOC_DE,SCM_Assoc_DE,2,"assoc/de") ;

/* This implementation does not limit the number of arguments to be passed
   but needs to walk twice the arguments.                                 */
SCM SCM_Apply (SCM args, SCM nullenv)
{ SCM function ;
  SCM frame, last_arg ;
  int size, i,index ;
  int number = args->frame.size-2 ;
  if ( !(number >= 2) ) SCM_error(28) ;
  function = args->frame.value[0] ;
  /* Count the number of arguments */
  size = number-2 ;
  last_arg = args->frame.value[number-1] ;
  while ( SCM_pairp(last_arg) )
    { size++ ;
      last_arg = SCM_cdr(last_arg) ; } ;
  /* Build a frame */
  frame = SCM_make_frame(1+size+1) ;
  for ( index=0, i=1 ; i<number-1 ; i++,index++ ) 
    frame->frame.value[index] = args->frame.value[i] ;
  last_arg = args->frame.value[number-1] ;
  while ( SCM_pairp(last_arg) )
    { frame->frame.value[index++] = SCM_car(last_arg) ;
      last_arg = SCM_cdr(last_arg) ; } ;
  if ( SCM_CAREFUL ) if ( !(SCM_nullp(last_arg)) ) SCM_error(27) ;
  return SCM_call(function,frame) ;
}
static struct SCM_primitive SCM_APPLY_object =
  {{SCM_FUNCTION_TAG, (SCM) NULL}, SCM_Apply };
SCM SCM_APPLY = (SCM) &(SCM_APPLY_object) ;

SCM SCM_listify(SCM frame, SIZE_T arity) {
  SIZE_T index = frame->frame.size-2 ;
  SCM result = SCM_empty ;
  for ( ; arity<index ; index-- ) 
    result = SCM_cons(frame->frame.value[index-1],result) ;
  frame->frame.value[arity] = result ;
}

/* End of rt.c */
