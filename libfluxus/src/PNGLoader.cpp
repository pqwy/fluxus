// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <iostream>
#include <cstdlib>
#include "png.h"
#include "OpenGL.h"
#include "PNGLoader.h"
#include "Trace.h"

using namespace Fluxus;
using namespace std;

void PNGLoader::Load(const string &Filename, TexturePainter::TextureDesc &desc)
{
	desc.ImageData = NULL;
	FILE *fp=fopen(Filename.c_str(),"rb");
	if (!fp || Filename=="")
	{
		Trace::Stream<<"Couldn't open image ["<<Filename<<"]"<<endl;
	}
	else
	{
		png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
		png_infop info_ptr = png_create_info_struct(png_ptr);

		if (setjmp(png_jmpbuf(png_ptr)))
		{
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			Trace::Stream<<"Error reading image ["<<Filename<<"]"<<endl;
			fclose(fp);
			return;
		}

		png_init_io(png_ptr, fp);
		png_read_info(png_ptr, info_ptr);

		unsigned long width = png_get_image_width(png_ptr, info_ptr);
		unsigned long height = png_get_image_height(png_ptr, info_ptr);
		int bit_depth = png_get_bit_depth(png_ptr, info_ptr);
		int colour_type = png_get_color_type(png_ptr, info_ptr);
		png_bytep *row_pointers=new png_bytep[height];
		unsigned int rb = png_get_rowbytes(png_ptr, info_ptr);

		for (unsigned long row=0; row<height; row++)
		{
			row_pointers[row] = new png_byte[rb];
		}

		// read the data into the row pointers
		png_read_image(png_ptr, row_pointers);
		fclose(fp);

		// make a new contiguous array to store the pixels
		desc.ImageData=new unsigned char[rb*height];
		int p=0;
		for (int row = height-1; row>=0; row--) // flip around to fit opengl
		{
			for (unsigned int i=0; i<rb; i++)
			{
				desc.ImageData[p]=(unsigned char)(row_pointers[row])[i];
				p++;
			}
		}

		// clear up the row_pointers
		for (unsigned long row=0; row<height; row++)
		{
			delete[] row_pointers[row];
		}
		delete[] row_pointers;

		// convert the format to fluxus texture format stuff
		desc.Width = width;
		desc.Height = height;

		switch (colour_type)
		{
			case PNG_COLOR_TYPE_RGB:
						desc.Format = desc.InternalFormat = GL_RGB;
						desc.Size = width * height * 3;
						break;
			case PNG_COLOR_TYPE_RGB_ALPHA:
						desc.Format = desc.InternalFormat = GL_RGBA;
						desc.Size = width * height * 4;
						break;
			default:
						Trace::Stream<<"PNG pixel format not supported : "<<(int)png_get_color_type(png_ptr, info_ptr)<<" "<<Filename<<endl;
						delete[] desc.ImageData;
						desc.ImageData=NULL;
						break;
        }

		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
	}
}

void PNGLoader::Save(const string &Filename, unsigned int w, unsigned int h, int pf, unsigned char *data)
{
	FILE *f;
	png_structp ppng;
	png_infop pinfo;
	png_text atext[3];
	unsigned int i;

	unsigned int numchannels = 3;
	if (pf==GL_RGBA) numchannels = 4;

	if (!(f = fopen (Filename.c_str(), "wb")))
	{
		Trace::Stream<<"Error writing png file"<<endl;
		return;
	}

	if (!(ppng = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL)))
	{
		Trace::Stream<<"Error writing png file"<<endl;
		fclose (f);
		return;
	}

	if (!(pinfo = png_create_info_struct (ppng)))
	{
		Trace::Stream<<"Error writing png file"<<endl;
		fclose (f);
		png_destroy_write_struct (&ppng, NULL);
		return;
	}

	if (setjmp (png_jmpbuf (ppng)))
	{
		Trace::Stream<<"Error writing png file"<<endl;
		fclose (f);
		png_destroy_write_struct (&ppng, &pinfo);
		return;
	}

	png_init_io (ppng, f);

	if (pf==GL_RGB)
	{
		png_set_IHDR (ppng, pinfo, w, h, 8, PNG_COLOR_TYPE_RGB,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
		PNG_FILTER_TYPE_BASE);
	}
	else if (pf==GL_RGBA)
	{
		png_set_IHDR (ppng, pinfo, w, h, 8, PNG_COLOR_TYPE_RGBA,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
		PNG_FILTER_TYPE_BASE);
	}
	else
	{
		Trace::Stream<<"Error, unknown pixel format"<<endl;
		fclose (f);
		png_destroy_write_struct (&ppng, NULL);
		return;
	}

	atext[0].key = const_cast<char *>("title");
	atext[0].text = const_cast<char *>("made with fluxus");
	atext[0].compression = PNG_TEXT_COMPRESSION_NONE;
	#ifdef PNG_iTXt_SUPPORTED
	atext[0].lang = NULL;
	atext[1].lang = NULL;
	#endif
	png_set_text (ppng, pinfo, atext, 2);
	png_write_info (ppng, pinfo);
	unsigned int stride=w*numchannels;
	{
		png_bytep *aprow = (png_bytep*) malloc(h * sizeof(png_bytep));
		for (i = 0; i < h; ++i) aprow[i] = data + stride * (h - 1 - i);	// flip Y for opengl
		png_write_image (ppng, aprow);
		free(aprow);
	}

	png_write_end (ppng, pinfo);
	png_destroy_write_struct (&ppng, &pinfo);
	fclose (f);
}

