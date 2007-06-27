// Copyright (C) 2007 Dave Griffiths
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

#include <assert.h>
#include "SchemeHelper.h"
#include "Engine.h"
#include "PDataFunctions.h"
#include "Renderer.h"
#include "FluxusEngine.h"

using namespace PDataFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// PrimitiveData
// Primitive data (pdata for short) is fluxus' name for data which comprises primitives. In polygon primitives this means 
// the vertex information, in particle primitives it corresponds to the particle information, in NURBS primitives it's 
// the control vertices. Access to pdata gives you the ability to use primitives which are otherwise not very 
// interesting, and deform and shape other primitives to give much more detailed models and animations. You can also add
// your own pdata, which is treated exactly like the built in types. Primitive data is named by type strings, the names of
// which depend on the sort of primitive. All pdata commands operate on the currently grabbed primitive.
// Example:
// ; a function to deform the points of an object
// (define (deform n)
//     (pdata-set! "p" n (vadd  (pdata-ref "p" n)                ; the original point, plus
//         (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 0.1)))     ; a small random vector
//     (if (zero? n)
//         0
//         (deform (- n 1))))
//     
// (hint-unlit) ; set some render settings to
// (hint-wire)  ; make things easier to see
// (line-width 4)
// (define myobj (build-sphere 10 10)) ; make a sphere
// (grab myobj)
// (deform (pdata-size)) ; deform it
// (ungrab)
// EndSectionDoc 

// StartFunctionDoc-en
// pdata-ref type-string index-number
// Returns: value-vector/colour/matrix/number
// Description:
// Returns the corresponding pdata element.
// Example:
// (pdata-ref "p" 1)
// EndFunctionDoc

Scheme_Object *pdata_ref(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret=NULL;
	
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();	
	ArgCheck("pdata-ref", "si", argc, argv);		
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size=0;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (type=='f')	
			{
				ret=scheme_make_double(Grabbed->GetData<float>(name,index%size)); 
			}
			else if (type=='v')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dVector>(name,index%size).arr(),3); 
			}
			else if (type=='c')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dColour>(name,index%size).arr(),4); 
			}
			else if (type=='m')	
			{
				ret=FloatsToScheme(Grabbed->GetData<dMatrix>(name,index%size).arr(),16); 
			}
			else
			{
				cerr<<"unknown pdata type ["<<type<<"]"<<endl;
			}
		
		}
		
		if (ret==NULL)
		{
			cerr<<"could not find pdata called ["<<name<<"]"<<endl;
  			MZ_GC_UNREG();
			return scheme_make_double(0);
		}
		
  		MZ_GC_UNREG();
		return ret;
	}			
	
	cerr<<"pdata-get called without an objected being grabbed"<<endl;
  	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pdata-set! type-string index-number value-vector/colour/matrix/number
// Returns: void
// Description:
// Writes to the corresponding pdata element.
// Example:
// (pdata-set! "p" 1 (vector 0 100 0))
// EndFunctionDoc

Scheme_Object *pdata_set(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();	
	ArgCheck("pdata-set!", "si?", argc, argv);		
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		string name=StringFromScheme(argv[0]);
		unsigned int index=IntFromScheme(argv[1]);
		unsigned int size;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (type=='f')	
			{
				if (SCHEME_NUMBERP(argv[2])) Grabbed->SetData<float>(name,index%size,FloatFromScheme(argv[2]));
				else cerr<<"expected number value in pdata-set"<<endl;
			}
			else if (type=='v')	
			{
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==3) 
				{
					dVector v;
					FloatsFromScheme(argv[2],v.arr(),3);
					Grabbed->SetData<dVector>(name,index%size,v);
				}
				else cerr<<"expected vector (size 3) value in pdata-set"<<endl;
			}
			else if (type=='c')	
			{
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])>=3 && SCHEME_VEC_SIZE(argv[2])<=4)
				{
					dColour c;
					if (SCHEME_VEC_SIZE(argv[2])==3) FloatsFromScheme(argv[2],c.arr(),3);
					else FloatsFromScheme(argv[2],c.arr(),4);
					Grabbed->SetData<dColour>(name,index%size,c);
				}
				else cerr<<"expected colour vector (size 3 or 4) value in pdata-set"<<endl;
			}
			else if (type=='m')	
			{
				if (SCHEME_VECTORP(argv[2]) && SCHEME_VEC_SIZE(argv[2])==16)
				{
					dMatrix m;
					FloatsFromScheme(argv[2],m.arr(),16);
					Grabbed->SetData<dMatrix>(name,index%size,m);
				}
				else cerr<<"expected matrix vector (size 16) value in pdata-set"<<endl;
			}
		}
	}
  	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pdata-add type-string name-string
// Returns: void
// Description:
// Adds a new user pdata array. Type is one of "v":vector, "c":colour, "f":float or "m":matrix.
// Example:
// (pdata-add "v" "mydata")
// (pdata-set "mydata" 0 (vector 1 2 3))
// EndFunctionDoc

Scheme_Object *pdata_add(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pdata-add", "ss", argc, argv);			
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string names=StringFromScheme(argv[0]);
		string types=StringFromScheme(argv[1]);
		char type=0;
		unsigned int size=0;
		
		PData *ptr=NULL;
		Grabbed->GetDataInfo("p", type, size);
		
		switch (types[0])
		{
			case 'v': ptr = new TypedPData<dVector>; ((TypedPData<dVector>*)ptr)->m_Data.resize(size); break;
			case 'c': ptr = new TypedPData<dColour>; ((TypedPData<dColour>*)ptr)->m_Data.resize(size); break;
			case 'f': ptr = new TypedPData<float>; ((TypedPData<float>*)ptr)->m_Data.resize(size); break;
			case 'm': ptr = new TypedPData<dMatrix>; ((TypedPData<dMatrix>*)ptr)->m_Data.resize(size); break;
			default : cerr<<"pdata-new: unknown type "<<types[0]<<endl; break;
		}
		
		if (ptr)
		{
			Grabbed->AddData(names,ptr);
		}
	}
	MZ_GC_UNREG(); 
	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-op funcname-string pdataname-string operator
// Returns: void
// Description:
// This is an experimental feature allowing you to do operations on pdata very quickly,
// for instance adding element for element one array of pdata to another. You can 
// implement this in Scheme as a loop over each element, but this is slow as the 
// interpreter is doing all the work. It's much faster if you can use a pdata-op as
// the same operation will only be one Scheme call.
// Example:
// (pdata-op "+" "mydata" (vector 1 2 3)) // add a vector to all the pdata vectors
// (pdata-op "+" "mydata" "myotherdata") // add two pdata vectors element for element
// (pdata-op "*" "mydata" (vector 1 2 3)) // multiply a vector to all the pdata vectors
// (pdata-op "*" "mydata" "myotherdata") // multiply two pdata vectors element for element
// (pdata-op "closest" "p" (vector 100 0 0)) // returns position of the closest vertex to this point
// (pdata-op "sin" "mydata" "myotherdata") // sine of one float pdata to another
// (pdata-op "cos" "mydata" "myotherdata") // cosine of one float pdata to another
// EndFunctionDoc

Scheme_Object *pdata_op(int argc, Scheme_Object **argv)
{
	DECL_ARGV(); 
	ArgCheck("pdata-op", "ss?", argc, argv);			
    PData *ret=NULL;
	
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string op=StringFromScheme(argv[0]);
		string pd=StringFromScheme(argv[1]);
		
		// find out what the inputs are, and call the corresponding function
		if (SCHEME_CHAR_STRINGP(argv[2]))
		{
			string operand=StringFromScheme(argv[2]);
			
			PData* pd2 = Grabbed->GetDataRaw(operand);
			
			TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(pd2);	
			if (data) ret = Grabbed->DataOp(op, pd, data);
			else
			{
				TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(pd2);
				if (data) ret = Grabbed->DataOp(op, pd, data);
				else 
				{
					TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(pd2);
					if (data) ret = Grabbed->DataOp(op, pd, data);
					else 
					{
						TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(pd2);
						if (data) ret = Grabbed->DataOp(op, pd, data);
					}
				}
			}
		}
		else if (SCHEME_NUMBERP(argv[2]))
		{
			ret = Grabbed->DataOp(op, pd, (float)FloatFromScheme(argv[2]));
		}
		else if (SCHEME_VECTORP(argv[2]))
		{
			switch (SCHEME_VEC_SIZE(argv[2]))
			{
				case 3:
				{
					dVector v;
					FloatsFromScheme(argv[2],v.arr(),3);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 4:
				{
					dColour v;
					FloatsFromScheme(argv[2],v.arr(),4);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 16:
				{
					dMatrix v;
					FloatsFromScheme(argv[2],v.arr(),16);
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;	
			}
		}
	}
		
	// convert the return data
	if (ret!=NULL)
	{
		TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(ret);	
		if (data) 
		{
			dVector r = data->m_Data[0];
			delete ret;
			MZ_GC_UNREG();
			return FloatsToScheme(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				MZ_GC_UNREG();
				return FloatsToScheme(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					MZ_GC_UNREG();
					return scheme_make_double(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						MZ_GC_UNREG();
						return FloatsToScheme(r.arr(),16);
					}
				}
			}
		}
	}
	MZ_GC_UNREG();
	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-copy pdatafrom-string pdatato-string
// Returns: void
// Description:
// Copies the contents of one pdata array to another. Arrays must match types.
// Example:
// (pdata-copy "p" "mydata") // copy the vertex positions to a user array
// EndFunctionDoc

Scheme_Object *pdata_copy(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("pdata-copy", "ss", argc, argv);			
  	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		string source=StringFromScheme(argv[0]);
		string dest=StringFromScheme(argv[1]);
		Grabbed->CopyData(source,dest);
	}
	MZ_GC_UNREG(); 	
	return scheme_void;
}

// StartFunctionDoc-en
// pdata-size 
// Returns: count-number
// Description:
// Returns the size of the pdata arrays (they must all be the same). This is mainly 
// used for iterating over the arrays.
// Example:
// (define (mashup n)
//     (pdata-set "p" n (vector (flxrnd) (flxrnd) (flxrnd))) ; randomise the vertex position
//     (if (zero? n)
//         0
//         (mashup (- n 1)))) ; loops till n is 0
//
// (define shape (build-sphere 10 10))
// (grab shape)
// (mashup (pdata-size)) ; randomise verts on currently grabbed primitive
// (ungrab)
// EndFunctionDoc

Scheme_Object *pdata_size(int argc, Scheme_Object **argv)
{
    Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) 
	{
		return scheme_make_integer_value(Grabbed->Size());
	}
    return scheme_void;
}

// StartFunctionDoc-en
// finalise
// Returns: void
// Description:
// Doesn't do anything anymore, I need to remove this :)
// Example:
// EndFunctionDoc

Scheme_Object *finalise(int argc, Scheme_Object **argv)
{
	return scheme_void;
}

// StartFunctionDoc-en
// recalc-normals smoothornot-number
// Returns: void
// Description:
// For polygon primitives only. Looks at the vertex positions and calculates the lighting normals for you 
// automatically. Call with "1" for smooth normals, "0" for faceted normals.
// Example:
// (define shape (build-sphere 10 10)) ; build a sphere (which is smooth by default)
// (grab shape)
// (recalc-normals 0) ; make the sphere faceted
// (ungrab)
// EndFunctionDoc

Scheme_Object *recalc_normals(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
	ArgCheck("recalc-normals", "i", argc, argv);			
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

void PDataFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("pdata-ref", scheme_make_prim_w_arity(pdata_ref, "pdata-ref", 2, 2), env);
	scheme_add_global("pdata-set!", scheme_make_prim_w_arity(pdata_set, "pdata-set!", 3, 3), env);
	scheme_add_global("pdata-add", scheme_make_prim_w_arity(pdata_add, "pdata-add", 2, 2), env);
	scheme_add_global("pdata-op", scheme_make_prim_w_arity(pdata_op, "pdata-op", 3, 3), env);
	scheme_add_global("pdata-copy", scheme_make_prim_w_arity(pdata_copy, "pdata-copy", 2, 2), env);
	scheme_add_global("pdata-size", scheme_make_prim_w_arity(pdata_size, "pdata-size", 0, 0), env);
	scheme_add_global("finalise", scheme_make_prim_w_arity(finalise, "finalise", 0, 0), env);
	scheme_add_global("recalc-normals", scheme_make_prim_w_arity(recalc_normals, "recalc-normals", 1, 1), env);
 	MZ_GC_UNREG(); 
}
