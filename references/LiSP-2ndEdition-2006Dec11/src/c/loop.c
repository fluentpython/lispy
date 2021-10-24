/* $Id
 * Essai de faire boucler cpp.  Ben non! cela ne boucle pas!!!
 */

#define LOOP(x) (x+LOOP(x+1))
#define BAR(x) (x+LOOP(x+1))

void main () {
  int i = 0;
  return BAR(i);
}
