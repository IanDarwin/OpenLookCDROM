/*
 * Written by Bret K. Thaeler
 *    Los Alamos National Labs (MEE-10)
 *    thaeler@hc.dspo.gov
 *
 *  extract_font:
 *		This program converts a vfont (a possibly produced by the
 *		NeWS program 'dumpfont') into a series of ascii files
 *		sutable for use with the suntools utility 'iconedit'.
 *		You can use this code to modify NeWS icons...
 */

#include <vfont.h>
#include <sys/file.h>
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	FILE *font_file, *output_file;
	int i, j, k, bits_to_read, row_rounded;
	long pos;
	struct header header;
	struct dispatch characters[NUM_DISPATCH];
	char buff[256];
	unsigned char *char_buff, *data_pointer, data, sub_data;

	char_buff = 0;

	if (!(argc >= 2)) {
		printf("Usage: %s font_list\n", argv[0]);
		exit(-1);
	}

	for(argc--, argv++; argc > 0; argc--, argv++) {
		if ((font_file = fopen(argv[0], "r")) == NULL) {
			printf("Can't open file '%s'.\n", argv[0]);
			continue;
		}

		if (fread(&header, sizeof(struct header), 1, font_file) != 1)
			goto error;

		if (header.magic != VFONT_MAGIC) {
			printf("Bad MAGIC number in file '%s'.\n", argv[0]);
			fclose(font_file);
			continue;
		}

		if (char_buff) {
			free(char_buff);
			char_buff = 0;
		}
		char_buff = (unsigned char *) malloc(header.maxx/8 * header.maxy);

		if (fread(characters, sizeof(struct dispatch), NUM_DISPATCH, font_file)
		  != NUM_DISPATCH)
			goto error;

		pos = ftell(font_file);

		for(i = 0; i < NUM_DISPATCH; i++) {
			if (characters[i].nbytes == 0)
				continue;

			fseek(font_file, (long)(pos + characters[i].addr), 0);

			sprintf(buff, "%s.%d", argv[0], i);
			if ((output_file = fopen(buff, "w")) == NULL) {
				printf("Can't open outputfile '%s'.\n", buff);
				continue;
			}

			if (fread(char_buff, characters[i].nbytes, 1, font_file) != 1) {
				printf("Can't read from input file '%s' character '%d'.\n",
				  argv[0], i);
				fclose(output_file);
				continue;
			}

			if (((characters[i].left + characters[i].right) % 8) != 0) {
				bits_to_read =
				((8 - ((characters[i].left + characters[i].right) % 8)) +
				  (characters[i].left + characters[i].right)) *
				  (characters[i].up + characters[i].down);
				row_rounded =
				  (8 - ((characters[i].left + characters[i].right) % 8)) +
					(characters[i].left + characters[i].right);
			} else {
				bits_to_read =
				  (characters[i].left + characters[i].right) *
				  (characters[i].up + characters[i].down);
				row_rounded = (characters[i].left + characters[i].right);
			}

			data_pointer = char_buff;

			fprintf(output_file,
"/* Format_version=1, Width=%d, Height=%d, Depth=1, Valid_bits_per_item=16\n */\n",
			  /*
			  (characters[i].left + characters[i].right),
			  */
			  row_rounded,
			  (characters[i].up + characters[i].down));

			while(1) {
				fprintf(output_file, "\t");
				for(k = 0; k < 8; k++) {
					fprintf(output_file, "0x%02X", *(data_pointer++));
					fprintf(output_file, "%02X", *(data_pointer++));

					if ((bits_to_read -= 16) < 0)
						bits_to_read = 0;

					if (bits_to_read != 0)
						fprintf(output_file, ",");
					else {
						fprintf(output_file, "\n");
						goto done;
					}
				}
				fprintf(output_file, "\n");
			}
done:

			fclose(output_file);
		}

		continue;
error:
		printf("Error while reading file '%s'.\n", argv[0]);
		if (char_buff) {
			free(char_buff);
			char_buff = 0;
		}
		fclose(font_file);
	}
}

