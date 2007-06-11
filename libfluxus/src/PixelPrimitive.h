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

#include "Primitive.h"

#ifndef N_PIXELPRIM
#define N_PIXELPRIM

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A pixel primitive - this allows you to make 
/// procedural textures, as the pixel values are
/// accessable as pdata in this primitive. The 
/// resulting texture can then be uploaded and 
/// applied to other primitives.
class PixelPrimitive : public Primitive
{
public:	
	PixelPrimitive(unsigned int w, unsigned int h);
	PixelPrimitive(const PixelPrimitive &other);
	virtual  ~PixelPrimitive();
	
	///////////////////////////////////////////////////
	///@name Primitive Interface
	///@{
	virtual PixelPrimitive* Clone() const;
	virtual void Render();
	virtual dBoundingBox GetBoundingBox();
	virtual void RecalculateNormals(bool smooth) {}
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "PixelPrimitive"; }
	///@}
	
	/// Upload the texture to the graphics card
	void Upload();
	
	/// Load a png file into this primitive
	///\todo add a save too
	void Load(const string &filename);
	
	/// Get the uploaded texture ID - call Upload() first.
	unsigned int GetTexture() { return m_Texture; }
	
protected:

	virtual void PDataDirty();
	
	vector<dVector> m_Points;
	vector<dColour> *m_ColourData;
	
	unsigned int m_Texture;
	
	unsigned int m_Width;
	unsigned int m_Height; 
};

};

#endif
