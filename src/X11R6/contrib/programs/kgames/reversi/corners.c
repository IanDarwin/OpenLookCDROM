/*
 *	cornerscores.c
 */

# include	"reversi.h"

scoreT cornerscores[4][4][4][4] = {
		-20,	/* O O O O */
		  0,	/* O O O - */
		 10,	/* O O O * */
		  0,	/* ignore  */

		-20,	/* O O - O */
		  0,	/* O O - - */
		-10,	/* O O - * */
		  0,	/* ignore  */

		-20,	/* O O * O */
		  0,	/* O O * - */
		 20,	/* O O * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		-20,	/* O - O O */
		  0,	/* O - O - */
		-10,	/* O - O * */
		  0,	/* ignore  */

		-20,	/* O - - O */
		  0,	/* O - - - */
		-10,	/* O - - * */
		  0,	/* ignore  */

		-20,	/* O - * O */
		  0,	/* O - * - */
		 20,	/* O - * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		-20,	/* O * O O */
		  0,	/* O * O - */
		 20,	/* O * O * */
		  0,	/* ignore  */

		-20,	/* O * - O */
		  0,	/* O * - - */
		 10,	/* O * - * */
		  0,	/* ignore  */

		-20,	/* O * * O */
		  0,	/* O * * - */
		 10,	/* O * * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		 40,	/* - O O O */
		  0,	/* - O O - */
		-40,	/* - O O * */
		  0,	/* ignore  */

		 40,	/* - O - O */
		  0,	/* - O - - */
		-40,	/* - O - * */
		  0,	/* ignore  */

		 40,	/* - O * O */
		  0,	/* - O * - */
		-40,	/* - O * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		 40,	/* - - O O */
		  0,	/* - - O - */
		-40,	/* - - O * */
		  0,	/* ignore  */

		 40,	/* - - - O */
		  0,	/* - - - - */
		-40,	/* - - - * */
		  0,	/* ignore  */

		 40,	/* - - * O */
		  0,	/* - - * - */
		-40,	/* - - * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		 40,	/* - * O O */
		  0,	/* - * O - */
		-40,	/* - * O * */
		  0,	/* ignore  */

		 40,	/* - * - O */
		  0,	/* - * - - */
		-40,	/* - * - * */
		  0,	/* ignore  */

		 40,	/* - * * O */
		  0,	/* - * * - */
		-40,	/* - * * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		-20,	/* * O O O */
		  0,	/* * O O - */
		 20,	/* * O O * */
		  0,	/* ignore  */

		-10,	/* * O - O */
		  0,	/* * O - - */
		 20,	/* * O - * */
		  0,	/* ignore  */

		-10,	/* * O * O */
		  0,	/* * O * - */
		 20,	/* * O * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		-10,	/* * - O O */
		  0,	/* * - O - */
		 20,	/* * - O * */
		  0,	/* ignore  */

		-10,	/* * - - O */
		  0,	/* * - - - */
		 20,	/* * - - * */
		  0,	/* ignore  */

		-10,	/* * - * O */
		  0,	/* * - * - */
		 20,	/* * - * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		-10,	/* * * O O */
		  0,	/* * * O - */
		 20,	/* * * O * */
		  0,	/* ignore  */

		-10,	/* * * - O */
		  0,	/* * * - - */
		 20,	/* * * - * */
		  0,	/* ignore  */

		-10,	/* * * * O */
		  0,	/* * * * - */
		 20,	/* * * * * */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */
		  0,	/* ignore  */

	};
