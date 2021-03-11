#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AFSox/RCS/af.c,v 1.5 1994/04/06 23:08:14 tml Exp $";
#endif /* RCS_ID */
#endif /* LINT */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#include <AF/AFlib.h>
#include <st.h>
#include <errno.h>

#define LASTCHAR '/'

extern char *strrchr(char *, int );
extern auwrite(), austartwrite(), austopwrite();
extern vocread();
extern bufread(), bufwrite(), bufcp(), nothing();
extern rawbuf(), rawread();
extern wavwrite(), wavbuf();



/*
        Initialize the soundstream structure, ft must be non NULL.  The
        other parameters are normally 0.  If a headerless file is to be
        opened then the other parameters must be filled in.  This list
        of params probably needs to be expanded.  This function must be
        called before AFOpenFileRead() or AFOpenFileWrite().
*/

void
AFInitFile(ft, type, rate, size, style, chan, mode, noHeaderFlag, func)
ft_t ft;                /* (struct soundstream *) */
char *type;             /* something like "au", "voc", "wav", or etc. */
int rate;               /* sampling rate */
int size;               /* BYTE, WORD, LONG, FLOAT, DOUBLE, or IEEE */
int style;              /* UNSIGNED, SIGN2, ULAW, or ALAW */
int chan;               /* number of channels */
int mode;		/* PLAY or RECORD */
int noHeaderFlag;       /* nonzero if header is absent */
void (*func)();		/* error handling routine */
{
	bzero(ft, sizeof(*ft));

	if (size)
		ft->info.size = size;
	else
		ft->info.size = -1;

	if (style)
		ft->info.style = style;
	else
		ft->info.style = -1;

	if (rate)
		ft->info.rate = rate;
	else
		ft->info.rate = -1;
	if (chan)
		ft->info.channels = chan;
	else
		ft->info.channels = 1;
	ft->info.mode = mode;
	ft->hasHeader = !noHeaderFlag;
	ft->outfo.rate = -1;
	ft->outfo.size = -1;
	ft->outfo.style = -1;
	ft->outfo.channels = -1;
	ft->seekable = 1;  /* temporary hack */
	ft->comment = (char *) 0;
	ft->swap = 0;
	ft->ibuf = (char *) 0;
	ft->isize = -1;
	ft->obuf = (char *) 0;
	ft->osize = -1;

	if (type)
		ft->filetype = type;
	else
		ft->filetype = (char *) 0;
	if (func)
		ft->errfunc = func;
	else
		ft->errfunc = NULL;
	ft->fp = 0;
	ft->filename = (char *) 0;
}
/*
	Use the filename suffix to determine the sound format
*/

int
AFGetType(formp)
ft_t formp;
{
        char **list;
        int i;

        if (! formp->filetype)
	   goto fail;
        if (! strcmpcase(formp->filetype, "snd")) {
		formp->filetype = "au";
	}
        for(i = 0; formats[i].names; i++) {
                for(list = formats[i].names; *list; list++) {
                        char *s1 = *list, *s2 = formp->filetype;
                        if (! strcmpcase(s1, s2))
                                break;  /* not a match */
                }
                if (! *list)
                        continue;
                /* Found it! */
                formp->h = formats[i];
                return AF_SUCCESS;
        }
fail:
	return AF_FAILURE;
}

/*
        Open the named file for reading.  Return AF_FAILURE if there is a 
	problem, AF_SUCCESS otherwise.  The type of the file will be 
	determined from the suffix.  An earlier call to AFInitFile() can 
	provide the suffix if its missing.  The header of the file, if 
	present, is read.  If AF_FAILURE is returned, errno is set to
	EACCES if the file could not be accessed and EINVAL if the file
	type is unknown or there is no header.
*/

int
AFOpenFileRead(name, ft)
char *name;
ft_t ft;
{
	FILE *fp;

	if ((fp = fopen(name, "r")) == 0)
	{
		errno = EACCES;
		return AF_FAILURE;
	}
	ft->fp = fp;
	ft->filename = name;

	if (ft->hasHeader)
	{
	   if (!ft->filetype) {
                if (ft->filetype = strrchr(name, LASTCHAR))
                        ft->filetype++;
                else
                        ft->filetype = name;
                if (ft->filetype = strrchr(ft->filetype, '.'))
                        ft->filetype++;
                else /* Default to "auto" */
                        ft->filetype = "auto";
           }
	}
	else
	   ft->filetype = "raw";
	ft->comment = ft->filename;
	if ((AFGetType(ft) == AF_FAILURE) || 
	    ((*(ft->h.startread))(ft) == AF_FAILURE))
	{
		errno = EINVAL;
		return AF_FAILURE;
	}
	return AF_SUCCESS;
}

/*
        Open the named file for writing.  Return AF_FAILURE if there is a 
	problem, AF_SUCCESS otherwise.  The type of the file will be 
	determined from the suffix.  An earlier call to AFInitFile() can 
	provide the suffix if its missing.  If the file is not a .raw file,
	the header is written during a successful open.  If AF_FAILURE is 
	returned, errno is set to EACCES if the file could not be created
	or opened for writing and EINVAL if the file type was unknown.
*/

int
AFOpenFileWrite(name, ft)
char *name;
ft_t ft;
{
	FILE *fp;

	if ((fp = fopen(name, "w")) == 0)
	{
		errno = EACCES;
		return AF_FAILURE;
	}
	ft->fp = fp;
	ft->filename = name;

	if (ft->hasHeader)
	{
	   if (!ft->filetype) {
                if (ft->filetype = strrchr(name, LASTCHAR))
                        ft->filetype++;
                else
                        ft->filetype = name;
                if (ft->filetype = strrchr(ft->filetype, '.'))
                        ft->filetype++;
                else /* Default to "auto" */
                        ft->filetype = "auto";
           }
	}
	else
	   ft->filetype = "raw";
	ft->comment = ft->filename;
	if (AFGetType(ft) == AF_FAILURE)
	{
		errno = EINVAL;
		return AF_FAILURE;
	}
	if ((*(ft->h.startwrite))(ft) == AF_FAILURE)
        {
                errno = EINVAL;
                return AF_FAILURE;
        }
	return AF_SUCCESS;
}

/*
        Close file.  Free all resources.
*/

AFCloseFile(ft)
ft_t ft;
{
	if (ft->info.mode == RECORDMODE)
		(*(ft->h.stopwrite))(ft);
	fclose(ft->fp);
	if (ft->ibuf)
		free(ft->ibuf);
	if (ft->obuf)
		free(ft->obuf);
}

/*

        Request to read inpnum samples.  Returns the actual number of input
        samples read.  The size of the output buffer (in bytes) is returned
        in outpsize.  Conversion from the input file type to the type of
        the ouput device is performed as well as any necessary resampling.

        The actual data read and converted will be accessible from

                ft->ibuf

*/

int
AFRead(ft, inpnum, outpsize)
ft_t ft;
int inpnum;
int *outpsize;
{
	int ret_val;
	long isamp, osamp, istart;
#if 0
	long i, idone, odone;
#endif
	char *obuf, *ib;
	char *tmp;
	int tmpsize;
	int osize;
	int tsize;

	isamp = inpnum;
	osamp = (ft->outfo.rate * isamp) / ft->info.rate;

	if (osamp*ft->outfo.channels > isamp*ft->info.channels)
		tsize = (osamp * ft->outfo.channels * sizeof(int));
	else
		tsize = (isamp * ft->info.channels * sizeof(int));

	if (ft->ibuf)
	{
		if (tsize > ft->isize)
		{
			if ((ft->ibuf = (char *) realloc(ft->ibuf, tsize)) == 0)
				return fail(ft, "memory allocation failed\n");
			ft->isize = tsize;
		}
	}
	else
	{
		if ((ft->ibuf = (char *) malloc(tsize)) == 0)
			return fail(ft, "memory allocation failed\n");
		ft->isize = tsize;
	}

	isamp = ret_val = (*(ft->h.read))(ft, ft->ibuf, inpnum);

	if (isamp <= 0)
		return 0;
	osamp = (ft->outfo.rate * isamp) / ft->info.rate;

	if (ft->obuf)
	{
		if (tsize > ft->osize)
		{
			if ((ft->obuf = (char *) realloc(ft->obuf, tsize)) == 0)
				return fail(ft, "memory allocation failed\n");
			ft->osize = tsize;
		}
	}
	else
	{
		if ((ft->obuf = (char *) malloc(tsize)) == 0)
			return fail(ft, "memory allocation failed\n");
		ft->osize = tsize;
	}
                /* Effect (i.e. rate change) may do different sizes I and O */
	(* ft->eff.h.flow)(ft, ft->ibuf, ft->obuf, &isamp, &osamp);
#if 0
	if (ft->eff.h.flow != nothing)
	{
		long idone, odone;
		long *ib = ft->ibuf;
		long *ob = ft->obuf;

		while (isamp) {
			idone = isamp*ft->info.channels;
			odone = osamp*ft->outfo.channels;
        		(* ft->eff.h.flow)(ft, ib, ob, &idone, &odone);
			/* 
			 * kludge! 	
			 * Effect is stuck.  Start over with new buffer.
			 * This can drop samples at end of file. 
			 * No effects currently do this, but it could happen.
			 */
			if (idone == 0) {
				int i;
				for(i = isamp - 1; i; i--)
					ft->ibuf[i] = ib[i];
				isamp = 0;
				break;
			}
			isamp -= idone/ft->info.channels;
			ib += idone;
			ob += odone;
		}
	}
#endif /* 0 */
        (* ft->eff.h.stop)(ft);
	(*(ft->h.cvt))(ft, ft->ibuf, osamp);
	(*(ft->h.write))(ft, ft->ibuf, osamp);
	*outpsize = osamp;
	return ret_val;
}

/*

        Request to write inpnum samples.  Returns the actual number of input
        samples written.  The number of output samples is returned
        in outpsize.  Conversion from the input file type to the type of
        the ouput device is performed as well as any necessary resampling.

*/

int
AFWrite(ft, inpnum, outpsize)
ft_t ft;
int inpnum;
int *outpsize;
{
	long isamp, osamp, istart;
	long i, idone, odone;
	char *obuf, *ib;
	char *tmp;
	int tmpsize;
	int osize;
	int tsize;

	isamp = inpnum;
	osamp = (ft->info.rate * isamp) / ft->outfo.rate;

	if (osamp*ft->outfo.channels > isamp*ft->info.channels)
		tsize = (osamp * ft->outfo.channels * sizeof(int));
	else
		tsize = (isamp * ft->info.channels * sizeof(int));

	if (ft->ibuf)
	{
		if (tsize > ft->isize)
		{
			if ((ft->ibuf = (char *) realloc(ft->ibuf, tsize)) == 0)
				return fail(ft, "memory allocation failed\n");
			ft->isize = tsize;
		}
	}
	else
	{
		if ((ft->ibuf = (char *) malloc(tsize)) == 0)
			return fail(ft, "memory allocation failed\n");
		ft->isize = tsize;
	}
	osize = tsize;

	if (ft->obuf)
	{
		if (osize > ft->osize)
		{
			if ((ft->obuf = (char *) realloc(ft->obuf, osize)) == 0)
				return fail(ft, "memory allocation failed\n");
			ft->osize = osize;
		}
	}
	else
	{
		if ((ft->obuf = (char *) malloc(osize)) == 0)
			return fail(ft, "memory allocation failed\n");
		ft->osize = osize;
	}
	(*(ft->h.cvt))(ft, isamp);
                /* Effect (i.e. rate change) may do different sizes I and O */
        (* ft->eff.h.flow)(ft, ft->obuf, ft->ibuf, &isamp, &osamp);
        (* ft->eff.h.stop)(ft);
	(*(ft->h.write))(ft, ft->ibuf, osamp);
	fwrite(ft->ibuf, osamp, ft->info.size*ft->info.channels, ft->fp);
	*outpsize = osamp;
	return inpnum;
}

/*

        Returns the audio device for the given connection that best suits the
        given file type

*/

int
AFFindDevice(aud, ft)
AFAudioConn *aud;
ft_t ft;
{
        AFDeviceDescriptor *aDev;
        int     i;
	int 	defaulti = -1;
	int	typei = -1;
	int n;
	unsigned int *freq, *chan, *type, *nbuf;

        for(i=0; i<(n=ANumberOfAudioDevices(aud)); i++) {
                aDev = AAudioDeviceDescriptor(aud, i);
                if(aDev->inputsFromPhone != 0 || aDev->outputsToPhone != 0)
			continue;
		if (TYPE(ft,aDev) == MU255)
		{
			if (defaulti < 0)
				defaulti = i;
		}
		if ((TYPE(ft,aDev) == ft->info.type) && 
		    (CHAN(ft,aDev) == ft->info.channels))
		{
			if (typei < 0)
				typei = i;
			if (abs(ft->info.rate - FREQ(ft,aDev)) < SAMPLE_RATE_DIFF)
				break;
		}
	}
	
	if (i >= n)
	{
		if (typei >= 0)
			i = typei;
		else if (defaulti >= 0)
			i = defaulti;
		else
			return AF_FAILURE;
                aDev = AAudioDeviceDescriptor(aud, i);
		ft->outfo.rate = FREQ(ft,aDev);
		ft->outfo.channels = CHAN(ft,aDev);
		checkeffect(ft);

		switch (TYPE(ft,aDev))
		{
		case ALAW:
			ft->outfo.size = BYTE;
			ft->outfo.style = ALAW;
		/************
			ft->h.startwrite = austartwrite;
			ft->h.write = auwrite;
			ft->h.stopwrite = austopwrite;
		*************/
			break;
		case LIN16:
			ft->outfo.size = WORD;
			ft->outfo.style = SIGN2;
			if (ft->info.mode == RECORDMODE)
			{
				ft->h.cvt = rawbuf;
			}
			else
			{
				ft->h.cvt = bufcp;
				ft->h.write = wavwrite;
			}
			break;
		case MU255:
		default:
			ft->outfo.size = BYTE;
			ft->outfo.style = ULAW;
			if (ft->info.mode == RECORDMODE)
			{
				ft->h.cvt = rawbuf;
			}
			else
			{
				ft->h.cvt = bufcp;
				ft->h.write = auwrite;
			}
		}
	}
	else
	{
		ft->outfo.rate = FREQ(ft,aDev);
		ft->outfo.channels = CHAN(ft,aDev);
		checkeffect(ft);
		/* we found a good match, do not need to resample or convert */
		switch (TYPE(ft,aDev))
		{
		case ALAW:
			ft->outfo.size = BYTE;
			ft->outfo.style = ALAW;
		/*************
			ft->h.write = bufwrite;
		*************/
			break;
		case LIN16:
			ft->outfo.size = WORD;
			ft->outfo.style = SIGN2;
			ft->h.cvt = nothing;
			ft->eff.h.flow = nothing;
			ft->h.read = bufread;
			ft->h.write = wavbuf;
			break;
		case MU255:
		default:
			ft->outfo.size = BYTE;
			ft->outfo.style = ULAW;
			ft->h.cvt = nothing;
			ft->eff.h.flow = nothing;
			ft->h.read = bufread;
			ft->h.write = nothing;
		}
	}
	if ((ft->info.mode == RECORDMODE) && (ft->info.rate < 0))
		ft->info.rate = ft->outfo.rate;
	if (ft->h.cvt == 0)
		ft->h.cvt = nothing;
        (* ft->eff.h.start)(ft);
	return i;
}

int
AFSetDevice(aud, ft, dev)
AFAudioConn *aud;
ft_t ft;
int dev;
{
        AFDeviceDescriptor *aDev;
        int     i;
	int n;
	extern auwrite(), austartwrite(), austopwrite();

	if ((0 <= dev) && (dev <= ANumberOfAudioDevices(aud)))
	{
                aDev = AAudioDeviceDescriptor(aud, dev);
		switch (aDev->playBufType)
		{
		case MU255:
		default:
			ft->outfo.rate = aDev->playSampleFreq;
			ft->outfo.size = BYTE;
			ft->outfo.style = ULAW;
			ft->outfo.channels = aDev->playNchannels;
			ft->h.startwrite = austartwrite;
			ft->h.write = auwrite;
			ft->h.stopwrite = austopwrite;
		}
		checkeffect(ft);
        	(* ft->eff.h.start)(ft);
		return dev;
	}
	else
		return AF_FAILURE;
}

/*
        Record num samples from device, ac.  Use buffer maitained in ft.
*/

AFRecordFile(ac, t, ft, num)
AC ac;
ATime t;
ft_t ft;
int num;
{
	int size = num * ft->info.size * ft->info.channels;

	if (ft->ibuf)
	{
		if (size > ft->isize)
		{
			if ((ft->ibuf = (char *) realloc(ft->ibuf, size)) == 0)
				return fail(ft, "memory allocation failed\n");
			ft->isize = size;
		}
	}
	else
	{
		if ((ft->ibuf = (char *) malloc(size)) == 0)
			return fail(ft, "memory allocation failed\n");
		ft->isize = size;
	}
	AFRecordSamples(ac, t, size, (unsigned char *)ft->ibuf, ABlock);
}

/* check that all settings have been given */
int
AFCheckFormat(ft)
ft_t ft;
{
        if (ft->info.rate == 0)
                return fail(ft, 
		   "Sampling rate for %s file was not given\n", ft->filename);
        if ((ft->info.rate < 100) || (ft->info.rate > 50000))
                return fail(ft, "Sampling rate %lu for %s file is bogus\n",
                        ft->info.rate, ft->filename);
        if (ft->info.size == -1)
         return fail(ft, 
           "Data size was not given for %s file\nUse one of -b/-w/-l/-f/-d/-D", 
             ft->filename);
        if (ft->info.style == -1)
                return fail(ft, 
                "Data style was not given for %s file\nUse one of -s/-u/-U/-A", 
                ft->filename);
        /* it's so common, might as well default */
        if (ft->info.channels == -1)
                ft->info.channels = 1;
        /*      return fail(ft, 
			"Number of output channels was not given for %s file",
                        ft->filename); */
	return AF_SUCCESS;
}
