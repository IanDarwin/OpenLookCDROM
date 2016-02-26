/* (C) Universitaet Passau 1986-1991 */
       

#include<pixrect/pixrect_hs.h>
#include<stdio.h>
#include<math.h>
#include<malloc.h>
#include"print.h"

 
 
   typedef struct WORK_SP {
                  int         *digit;
                  int         value1,
                              value2,value3,
                              value4;
                  int         sum;
                  } work_sp;

   extern     Pixrect       *w_bitmap;
   static     wi_out_str    *pr_set;
   static     FILE          *esc_file; 
   static     work_sp       *working_array;



   static    int            perm[16],
                            colour[16],
                            colourtable[16];
   static    int            latest_colour;

   static    double         paper_width,
                            paper_length;
   static    int            needle_number;
   static    char*          file_name;

   static    int            modulation,
                            n1,n2;

   static    char           decision1,
                            decision2,
                            decision3;
  
      
             char            *calloc();
             FILE            *fopen();
             Pixrect         *pr_open();
            

        
   /********************************************************************************************************************/
/********************************************************************************************************************/

 Create_storage(bm_width)
        int           bm_width;

{

   int    rv1;

   working_array = ((work_sp *)calloc((unsigned)bm_width,(sizeof(struct WORK_SP))));  
   for (rv1 = 0;rv1 < bm_width;rv1++)
       working_array[rv1].digit = ((int*)calloc(needle_number,sizeof(int)));

}

/*********************************************************************************************************************/
/*********************************************************************************************************************/



Free_storage(bm_width)
 int     bm_width;

{
   int    rv1;

   for (rv1 = 0;rv1 < bm_width;rv1++)
                cfree((char*)working_array[rv1].digit);
           cfree((char*)working_array);

}

/***********************************************************************************************************************/
/***********************************************************************************************************************/

Compute_graph_datas(bm_width,precision,col)
  int               bm_width,
                    precision,
                    col;

{
              int          value[36];
              unsigned     v1,v2,v3,v4;
              int          column_value,
                           rv2,rv3,rv4,
                           empty_column,
                           memo1,memo2,difference;

          for (rv2 = 0;rv2 < bm_width;rv2++)   {
               v1 = v2 = v3 = v4 = 0;
               for (rv3 = 0;rv3 < needle_number;rv3++) {
                   if (decision3 == 'm') {
                      if (working_array[rv2].digit[rv3] != 0)
                         value[rv3] = 1;
                         else value[rv3] = 0;
                   }  
                      else  if (decision3 == 'c') {
                               if (working_array[rv2].digit[rv3] == perm[col])
                                  value[rv3] = 1;
                                  else value[rv3] = 0;
                               }
               }

               for (rv3 = 0;rv3 < 8;rv3++)
                   if (value[rv3] == 1) 
                      v1 |=  1 << (7-rv3);
                   working_array[rv2].value1 = v1;
                   column_value = v1;
               if (needle_number >= 24)  {
                  for (rv3 = 8,rv4 = 0;rv3 < 16;rv3++,rv4++)
                      if (value[rv3] == 1)
                      v2 |= 1 << (7-rv4);
                  for (rv3 = 16,rv4 = 0;rv3 < 24;rv3++,rv4++)
                      if (value[rv3] == 1)
                         v3 |= 1 << (7-rv4);
                  working_array[rv2].value2 = v2;
                  working_array[rv2].value3 = v3;
                  column_value += (v2 + v3);
               }
               if (needle_number >= 36)  {
                  for (rv3 = 24,rv4 = 0;rv3 < 36;rv3++,rv4++)
                      if (value[rv3] == 1)
                         v4 |= 1 << (7-rv4);
                  working_array[rv2].value4 = v4;
                  column_value += v4;
               }   
            working_array[rv2].sum = column_value;
          }


            if (precision == 0) { 
               rv2 = 0;
               empty_column = 0;
               while (rv2 < bm_width)  {
                     memo1 = rv2;
                     if (working_array[rv2].sum == 0)  {
                        empty_column++;
                        rv2++;
                     } 
                     else  if (working_array[rv2].sum > 0)  {
                              if (empty_column > 0)  {
                                 if (needle_number == 8)
                                    empty_column *= 2;
                                 n2 = compute_n2(empty_column);
                                 n1 = empty_column - n2*256;
                                 fprintf(esc_file,"%c%c%c%c",27,'\\',n1,n2);
                                 empty_column = 0;
                                 memo1 = rv2;
                              } 
                              while (working_array[rv2].sum > 0  && rv2 < bm_width)  {
                                    memo2 = rv2;
                                    rv2++;
                              }
                              difference = memo2 - memo1 + 1;
                              n2 = compute_n2(difference);
                              n1 = difference - n2*256;
                              fprintf(esc_file,"%c%c%c%c%c",27,'*',modulation,n1,n2);
                              for (rv3 = memo1;rv3 <= memo2;rv3++)  { 
                                  fprintf(esc_file,"%c",working_array[rv3].value1);
                                  if (needle_number >=24) {
                                     fprintf(esc_file,"%c",working_array[rv3].value2);
                                     fprintf(esc_file,"%c",working_array[rv3].value3);
                                  }
                                  if (needle_number >= 36)
                                     fprintf(esc_file,"%c",working_array[rv3].value4);
                              }
                     }
                   
               } 
           }
           else if (precision == 1)  {                                      
                   fprintf(esc_file,"%c%c%c%c%c",27,'*',modulation,n1,n2);
                   for (rv3 = 0;rv3 < bm_width;rv3++) {
                       fprintf(esc_file,"%c",working_array[rv3].value1);
                       if (needle_number >= 24)  {
                          fprintf(esc_file,"%c",working_array[rv3].value2);
                          fprintf(esc_file,"%c",working_array[rv3].value3);
                       }
                       if (needle_number >= 36)  
                          fprintf(esc_file,"%c",working_array[rv3].value4);   
                   }
           } 
           fprintf(esc_file,"%c",13);
        

}


/******************************************************************************************/
/******************************************************************************************/


Create_escape_sequence(bm_width,precision)
 int       bm_width,
           precision;

{
    int    rv1;

    Row_colours(bm_width);

    if (decision3 == 'm')
        Compute_graph_datas(bm_width,precision,rv1);
        else {
              for (rv1 = 1;rv1 < 16;rv1++)  
                  if (colour[perm[rv1]] == 1)  {
                     if (latest_colour != perm[rv1]) 
                        fprintf(esc_file,"%c%c%c",27,'r',colourtable[perm[rv1]]);
                     latest_colour = perm[rv1];
                     Compute_graph_datas(bm_width,precision,rv1);
                  }
        }
                             
} 

/*********************************************************************************************************************/
/*********************************************************************************************************************/


graph_colour_print1(bitmap,bm_width,bm_pitch,precision)
 Pixrect    *bitmap;
 int        bm_width,bm_pitch,
            precision;

{

    int     begin,end,
            row,column;   
  

    /* Initialisierung der benoetigten Variablen  */
    begin = 0;
    end = begin + needle_number;
    column = 0;

    if (precision == 1) {     
       n2 = compute_n2(bm_width);
       n1 = bm_width - n2*256;
    }

    while (begin < bm_pitch)   { 
          for (column = 0;column < bm_width;column++)  {
              for (row = begin;row < end;row++)   
                  if (row < bm_pitch)
                     working_array[column].digit[row%needle_number] = pr_get(bitmap,column,row);
                     else  working_array[column].digit[row%needle_number] = 0;
           
            }

           Create_escape_sequence(bm_width,precision);
      
           begin += needle_number;
           end = begin + needle_number;
           fprintf(esc_file,"%c",10);
           /*message(" Done %.0f percent\n",((float)begin/(float)bm_pitch)*100);*/
     }

     fprintf(esc_file,"%c",13);

}  

/********************************************************************************************************************/
/********************************************************************************************************************/
                       
                           
graph_colour_print2(help_gr_bm,bm_width,bm_pitch,precision)
 Pixrect     *help_gr_bm; 
 int         bm_width,bm_pitch,
             precision;

{
   
    struct pr_prpos block;
    int begin,end;
    int row,column;   
  

    /* Initialisierung der benoetigten Variablen  */
    begin = 0;
    end = begin + needle_number;
    column = 0;

    if (precision == 1) {
       n2 = compute_n2(bm_width);
       n1 = bm_width - n2*256;
    }
   
    while (begin < bm_pitch)   { 
          for (column = 0;column < bm_width;column++)  {
              for (row = begin;row < end;row++)   
                  if (row < bm_pitch)  {
                     block.pr = help_gr_bm;
                     block.pos.x = column;
                     block.pos.y = row;
                     working_array[column].digit[row%needle_number] = prs_get(block);
                  } 
                     else  working_array[column].digit[row%needle_number] = 0;
           
            }

           Create_escape_sequence(bm_width,precision);
      
           begin += needle_number;
           end = begin + needle_number;
           fprintf(esc_file,"%c",10);
     }

     fprintf(esc_file,"%c",13);

}  

/*********************************************************************************************************************/
/*********************************************************************************************************************/
  


   
Row_colours(bm_width)
 int        bm_width;

{
   int rv1,rv2;

   
   for (rv1 = 0;rv1 < 16;rv1++)
       colour[rv1] = 0;
   for (rv1 = 0;rv1 < bm_width;rv1++)
       for (rv2 = 0;rv2 < needle_number;rv2++)
       colour[working_array[rv1].digit[rv2]] = 1;
}


/*********************************************************************************************************************/
/*********************************************************************************************************************/



permut_tab()

{ 
   perm[1] = 5;    /*   gelb                                HELL                   */
   perm[2] = 13;   /*   magenta                               .                    */
   perm[3] = 10;   /*                                         .                    */
   perm[4] = 6;    /*   cyan                                                       */
   perm[5] = 4;    /*                                                              */
   perm[6] = 3;    /*                                                              */
   perm[7] = 8;   /*                                                              */
   perm[8] = 1;    /*   orange                                                     */
   perm[9] = 2;    /*   gruen                                                      */
   perm[10] = 14;   /*   violett                                                    */
   perm[11] = 13;  /*                                                              */
   perm[12] = 12;  /*                                                              */
   perm[13] = 7;   /*   schwarz                                                    */
   perm[14] = 15;   /*                                         .                    */
   perm[15] = 9;  /*   braun                               DUNKEL                 */      
    
}

/*********************************************************************************************************************/
/*********************************************************************************************************************/


colour_tab()
{
   /*******************************************************************/
   /**                                                               **/
   /**     Druckerfarben           zugehoerige Farbwerte             **/
   /**                                                               **/
   /**     Schwarz                         0                         **/
   /**     Magenta                         1                         **/
   /**     Cyan                            2                         **/
   /**     Violett                         3                         **/
   /**     Gelb                            4                         **/
   /**     Orange                          5                         **/
   /**     Gruen                           6                         **/
   /**     Braun                           7                         **/
   /**                                                               **/
   /*******************************************************************/
   colourtable[1] = 5;
   colourtable[2] = 6;
   colourtable[3] = 2;
   colourtable[4] = 2;
   colourtable[5] = 4;
   colourtable[6] = 2;
   colourtable[7] = 0;
   colourtable[8] = 2;
   colourtable[9] = 7;
   colourtable[10] = 1;
   colourtable[11] = 3;
   colourtable[12] = 3;
   colourtable[13] = 1;
   colourtable[14] = 3;
   colourtable[15] = 0;
   
} 
 

/*********************************************************************************************************************/
/*********************************************************************************************************************/ 


compute_n2(x)
 int     x;

{
   int      out,
            value;

   out = 0;
   value = 0;
   while (value < x)  {
         value += 256;
         out++;  
   }
   return(out-1);
}

 

/*********************************************************************************************************************/
/*********************************************************************************************************************/



Scale_bitmap(bitmap,pr_set)
 Pixrect      *bitmap;
 wi_out_str   *pr_set;

{     

#define     NO 0
#define     YES 1
  
             int         bm_width,bm_pitch,
                         precision,  
                         feed,
                         print_density;
             float       compr1,compr2,
                         compression;


             Pixrect     *help_gr_bm;
             int         width,pitch, 
                         run1,run2;
    
             Pixrect     *help_li_bm;
    struct   pr_prpos    block;
             int         w,h,wh,hh,
                         comp_zeile,comp_spalte,comp_value,     
                         rv1,rv2,rv3,
                         hrv1,hrv2,
                         h_tab[16],
                         is_zero;
             char*       word1;
             char*       word2;
             char*       word3;
             int         page;
             int         corecture = 1;


    
     permut_tab();
     colour_tab(); 

     paper_width = pr_set->width;
     paper_length = pr_set->length;
     needle_number = pr_set->count;
     decision1 = pr_set->dec1;
     decision2 = pr_set->dec2;
     decision3 = pr_set->dec3;
     file_name = pr_set->name;


     if ((esc_file = fopen(file_name,"w")) == NULL) {       
        error(" Can't open file %s for writing", file_name);
        return;
        }
     fprintf(esc_file,"%c%c",27,'@');
     if (decision1 == 'y')
        word1 = "QUALITY";
        else word1 = "SPEED";
     if (decision2 == 'y')
        word2 = "ON_ONE_PAGE";
        else word2 = "ON_MORE_PAGES";
     if (decision3 == 'm')
        word3 = "MONOCHROME";
        else word3 = "COLOUR";
     
     fprintf(esc_file,"%c%c%c",27,'U',1);
      
             


    /* Berechnung der Groesse des skalierten Bitmap */
 
     bm_width = bitmap->pr_size.x;            /*   __________ width    */
     bm_pitch = bitmap->pr_size.y;            /*   |                   */
                                              /*   |                   */
                                              /*   |                   */
                                              /*   | pitch             */
 

    /*printf("Breite = %d   Hoehe = %d\n",bm_width,bm_pitch);*/

    
    if (needle_number == 8) {
       modulation = 0;
       print_density = 60;
       feed = 24;
       fprintf(esc_file,"%c%c",27,'P');
       fprintf(esc_file,"%c%c%c",27,'x',0);
    }
       else if (needle_number == 24) {
               modulation = 39;
               print_density = 180;
               feed = 24;
               fprintf(esc_file,"%c%c",27,'M');
               fprintf(esc_file,"%c%c%c",27,'x',1);
       }
       /*else if (needle_number == 36)*/
               /*modulation = ...  in vorliegendem Handbuch nicht angegeben*/
               /*print_density = ... */
               /*feed = ... */
               /*fprintf(esc_file,"%c%c",27,'..');*/ 

    fprintf(esc_file,"Print-Setting: %d Needles,   %s  , %s  ,  %s  ",needle_number,word1,word2,word3);
    fprintf(esc_file,"%c",10);
    fprintf(esc_file,"%c%c%c",27,'3',feed);
    fprintf(esc_file,"%c",10);

    if (decision1 == 'y')
        precision = YES;
        else precision = NO;     
                 
     
    if ((needle_number == 8) && ((double)bm_width <= fabs(paper_width*print_density)) && ((double)bm_pitch <= (paper_length-corecture)*print_density-2*feed)) {
       Create_storage(bm_width);
       graph_colour_print1(bitmap,bm_width,bm_pitch,precision);
       Free_storage(bm_width);
    }
       else if ((needle_number == 24) & ((double)bm_width <= fabs(paper_width*print_density)) && ((double)bm_pitch <= fabs(paper_length-corecture)*print_density-2*feed))  
               {
                  Create_storage(bm_width); 
                  graph_colour_print1(bitmap,bm_width,bm_pitch,precision);
                  Free_storage(bm_width);
            }
        
       else  {
          compr1 = bm_width / (paper_width*print_density); 
          compr2 = bm_pitch /  (paper_length*print_density);
          if (compr1 >= compr2)
             compression = compr1;
             else compression = compr2;

         if (decision2 == 'n') {
            message("Printer-output on more than one page (sorted:\n ");
            message("first y-direction, seconed x-direction)\n\n");
            page = 1;
            for (run1 = 0;run1 < bm_width;run1 += (int)fabs(paper_width*print_density))
                for (run2 = 0;run2 < bm_pitch;run2 += (int)fabs((paper_length-corecture)*print_density-3*feed),page++) {
                    if ((double)(bm_width - run1) < fabs(paper_width*print_density))
                       width = bm_width - run1;
                       else width = (int)fabs(paper_width*print_density);
                    if ((double)(bm_pitch - run2) < fabs((paper_length-corecture)*print_density-3*feed))
                       pitch = bm_pitch - run2;
                       else pitch = (int)fabs((paper_length-corecture)*print_density-3*feed);
                  
                    if (run1 < bm_width && run2 < bm_pitch) {
                       fprintf(esc_file,"%c%c%c",27,'r',0);
                       fprintf(esc_file,"PAGE %d",page);
                       fprintf(esc_file,"%c%c%c",27,'r',latest_colour);
                       fprintf(esc_file,"%c",10);
                       Create_storage(width);
                       help_gr_bm = pr_region(bitmap,run1,run2,width,pitch);
                       graph_colour_print2(help_gr_bm,width,pitch,precision);
                       close(help_gr_bm);
                       Free_storage(width);
                       fprintf(esc_file,"%c",12);
                    }
                }
         }   
            else if (decision2 == 'y')  {
                    message("Compression with factor %.0f\n",compression+1);
                    if (compression < 2.0)  {  w = 2;
                                               h = 2;  }
                    else if (compression < 3.0)  {  w = 3;
                                                    h = 3;  }
                    else if (compression < 4.0)  {  w = 4;
                                                    h = 4;  }
                    else if (compression < 5.0)  {  w = 5;
                                                    h = 5;  }
                    else {
                         error("Compression more than 5.0 is not supported\n");
                         return;
                    }
          
                   if (compression < 5.0) {
                      wh = w;
                      hh = h; 
                      comp_zeile = 0;
                      comp_spalte = 0;     
                      for (rv1 = 0;rv1 < bm_width;rv1 += w,comp_zeile++) {
                          comp_spalte = 0;
                          for (rv2 = 0;rv2 < bm_pitch;rv2 += h,comp_spalte++) {
                              if ((bm_width - rv1) < w)
                                 w = bm_width - rv1;
                                 else w = wh;
                              if ((bm_pitch -rv2) < h)
                                 h = bm_pitch - rv2 ;
                                 else h = hh; 

                              help_li_bm = pr_region(bitmap,rv1,rv2,w,h);
                              is_zero = YES;
                              for (rv3 = 0;rv3 < 16;rv3++)  h_tab[rv3] = 0;
                  
                              for (hrv1 = 0;hrv1 < w;hrv1++)
                                  for(hrv2 = 0;hrv2 < h;hrv2++)  {
                                  block.pr = help_li_bm;
                                  block.pos.x = hrv1;
                                  block.pos.y = hrv2;
                                  if (prs_get(block) != 0)  {
                                     is_zero = NO;
                                     h_tab[prs_get(block)]++;
                                  }
                              }
                              if (is_zero == NO) {
                                 comp_value = 0;
                                 for (rv3 = 1;rv3 < 16;rv3++) 
                                     if (h_tab[rv3] > h_tab[comp_value])
                                     comp_value = rv3;
                              }
                                 else comp_value = 0;
                              pr_put(bitmap,comp_zeile,comp_spalte,comp_value);
                              pr_close(help_li_bm);
                         }  
                      }
                      bm_width = comp_zeile;
                      bm_pitch = comp_spalte;
                      Create_storage(bm_width);
                      graph_colour_print1(bitmap,bm_width,bm_pitch,precision);
                      Free_storage(bm_width);
                   }   
            }         
       }         
       fprintf(esc_file,"%c%c",27,'@');
       fclose(esc_file);
}
 
/*********************************************************************************************************************/
/*********************************************************************************************************************/


p7_c_print(w_bitmap,pr_set)
 Pixrect   *w_bitmap;
 wi_out_str *pr_set;

{
   latest_colour = -1;
   Scale_bitmap(w_bitmap,pr_set);
}
