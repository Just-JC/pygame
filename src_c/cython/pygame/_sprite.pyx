##    pygame - Python Game Library
##    Copyright (C) 2000-2003, 2007  Pete Shinners
##              (C) 2004 Joe Wreschnig
##    This library is free software; you can redistribute it and/or
##    modify it under the terms of the GNU Library General Public
##    License as published by the Free Software Foundation; either
##    version 2 of the License, or (at your option) any later version.
##
##    This library is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##    Library General Public License for more details.
##
##    You should have received a copy of the GNU Library General Public
##    License along with this library; if not, write to the Free
##    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
##    Pete Shinners
##    pete@shinners.org

# cython: language_level=2
"""pygame module with basic game object classes

This module contains several simple classes to be used within games. There
are the main Sprite class and several Group classes that contain Sprites.
The use of these classes is entirely optional when using Pygame. The classes
are fairly lightweight and only provide a starting place for the code
that is common to most games.

The Sprite class is intended to be used as a base class for the different
types of objects in the game. There is also a base Group class that simply
stores sprites. A game could create new types of Group classes that operate
on specially customized Sprite instances they contain.

The basic Sprite class can draw the Sprites it contains to a Surface. The
Group.draw() method requires that each Sprite have a Surface.image attribute
and a Surface.rect. The Group.clear() method requires these same attributes
and can be used to erase all the Sprites with background. There are also
more advanced Groups: pygame.sprite.RenderUpdates() and
pygame.sprite.OrderedUpdates().

Lastly, this module contains several collision functions. These help find
sprites inside multiple groups that have intersecting bounding rectangles.
To find the collisions, the Sprites are required to have a Surface.rect
attribute assigned.

The groups are designed for high efficiency in removing and adding Sprites
to them. They also allow cheap testing to see if a Sprite already exists in
a Group. A given Sprite can exist in any number of groups. A game could use
some groups to control object rendering, and a completely separate set of
groups to control interaction or player movement. Instead of adding type
attributes or bools to a derived Sprite class, consider keeping the
Sprites inside organized Groups. This will allow for easier lookup later
in the game.

Sprites and Groups manage their relationships with the add() and remove()
methods. These methods can accept a single or multiple group arguments for
membership.  The default initializers for these classes also take a
single group or list of groups as argments for initial membership. It is safe
to repeatedly add and remove the same Sprite from a Group.

While it is possible to design sprite and group classes that don't derive
from the Sprite and AbstractGroup classes below, it is strongly recommended
that you extend those when you create a new Sprite or Group class.

Sprites are not thread safe, so lock them yourself if using threads.

"""

##todo
## a group that holds only the 'n' most recent elements.
## sort of like the GroupSingle class, but holding more
## than one sprite
##
## drawing groups that can 'automatically' store the area
## underneath so they can "clear" without needing a background
## function. obviously a little slower than normal, but nice
## to use in many situations. (also remember it must "clear"
## in the reverse order that it draws :])
##
## the drawing groups should also be able to take a background
## function, instead of just a background surface. the function
## would take a surface and a rectangle on that surface to erase.
##
## perhaps more types of collision functions? the current two
## should handle just about every need, but perhaps more optimized
## specific ones that aren't quite so general but fit into common
## specialized cases.

import pygame
from pygame import Rect, Surface
from pygame.time import get_ticks


from operator import truth
from libc.math cimport fabs, fmin, fmax, sin, cos, atan2, pi
from sys import version

from ._sdl2.sdl2 cimport *
from cpython cimport PyObject_CallFunctionObjArgs, PyDict_SetItem, \
    PyObject, PyList_SetSlice, PyList_GET_ITEM, PyTuple_GET_ITEM, PyDict_GetItem, PyDict_GetItemString, PyDict_Contains

from cpython.version cimport PY_MAJOR_VERSION

from cpython.mem cimport PyMem_Malloc, PyMem_Realloc, PyMem_Free

cimport cython
from cython.operator import dereference
#from cyrandom.cyrandom cimport random as cyrandom

# Python 3 does not have the callable function, but an equivalent can be made
# with the hasattr function.
if 'callable' not in dir(__builtins__):
    callable = lambda obj: hasattr(obj, '__call__')

# Don't depend on pygame.mask if it's not there...
try:
    from pygame.mask import from_surface
except:
    pass


#IF int(sys.version[0]) > 2:
    #cdef extern from "Python.h":
    #    cdef int Py_EQ
    #
    #    PyObject* PyUnicode_RichCompare(PyObject *left, PyObject *right, int op)



cdef extern from "limits.h":
    int INT_MIN
    int INT_MAX
    
    int SHRT_MIN
    int SHRT_MAX

cdef extern from "float.h":
    float FLT_MIN
    float FLT_MAX

#cdef extern from "_pygame.h" nogil:
#    ctypedef struct GAME_Rect:
#        int x
#        int y
#        int w
#        int h
#
#    ctypedef class pygame.Rect [object pgRectObject]:
#        cdef GAME_Rect r
#        cdef object weakreflist
#

cdef extern from "SDL.h" nogil:             # Exposed more SDL2 functions and structs from video.pxd
    ctypedef struct SDL_Rect:
        int x, y
        int w, h

    ctypedef struct SDL_FRect:
        float x, y
        float w, h

    ctypedef struct SDL_Point:
        int x, y

    ctypedef struct SDL_FPoint:
        float x, y

    ctypedef struct SDL_Texture
    ctypedef struct SDL_Renderer

    ctypedef enum SDL_RendererFlip:
        SDL_FLIP_NONE,
        SDL_FLIP_HORIZONTAL,
        SDL_FLIP_VERTICAL

    ctypedef enum SDL_BlendMode:
        SDL_BLENDMODE_NONE = 0x00000000,
        SDL_BLENDMODE_BLEND = 0x00000001,
        SDL_BLENDMODE_ADD = 0x00000002,
        SDL_BLENDMODE_MOD = 0x00000004,
        SDL_BLENDMODE_INVALID = 0x7FFFFFFF
    
    int SDL_RenderCopy(SDL_Renderer*   renderer,
                       SDL_Texture*    texture,
                       const SDL_Rect* srcrect,
                       const SDL_Rect* dstrect)
    int SDL_RenderCopyEx(SDL_Renderer*          renderer,
                         SDL_Texture*           texture,
                         const SDL_Rect*        srcrect,
                         const SDL_Rect*        dstrect,
                         const double           angle,
                         const SDL_Point*       center,
                         const SDL_RendererFlip flip)

    int SDL_RenderCopyF(SDL_Renderer*   renderer,
                       SDL_Texture*    texture,
                       const SDL_Rect* srcrect,
                       const SDL_FRect* dstrect)
    int SDL_RenderCopyExF(SDL_Renderer*         renderer,
                         SDL_Texture*           texture,
                         const SDL_Rect*        srcrect,
                         const SDL_FRect*       dstrect,
                         const double           angle,
                         const SDL_FPoint*      center,
                         const SDL_RendererFlip flip)


    int SDL_GetTextureAlphaMod(SDL_Texture* texture,
                               Uint8*       alpha)
    int SDL_SetTextureAlphaMod(SDL_Texture* texture,
                               Uint8        alpha)

    int SDL_GetTextureColorMod(SDL_Texture* texture,
                               Uint8*       r,
                               Uint8*       g,
                               Uint8*       b)
    int SDL_SetTextureColorMod(SDL_Texture* texture,
                               Uint8        r,
                               Uint8        g,
                               Uint8        b)

    int SDL_SetRenderTarget(SDL_Renderer* renderer,
                            SDL_Texture*  texture)


cdef extern from "pygame.h" nogil:  # Better access to pygame.Rect internals

    int pgRect_Check(object rect)
    SDL_Rect *pgRect_FromObject(object obj, SDL_Rect *temp)
    object pgRect_New(SDL_Rect *r)
    object pgRect_New4(int x, int y, int w, int h)
    SDL_Rect pgRect_AsRect(object rect)
    void import_pygame_rect()

    ctypedef class pygame.Rect [object pgRectObject]:
        cdef SDL_Rect r
        cdef object weakreflist

    
import_pygame_rect()


from ._sdl2.video cimport *
from ._sdl2 import error
from ._sdl2 import error as errorfnc


cdef inline double clamp_double(double a, double mn=0.0, double mx=1.0) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    return mn if a < mn else mx if a > mx else a


cdef inline double clamp_float(float a, float mn=0.0, float mx=1.0) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    return mn if a < mn else mx if a > mx else a


cdef inline int clamp_double_to_int(double a, double mn=0.0, double mx=1.0) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    return <int>mn if a < mn else <int>mx if a > mx else <int>a

cdef inline float clamp_double_to_float(double a, double mn=0.0, double mx=1.0) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    #a = ( a > mn ) * a + (not ( a > mn )) * mn
    #return <float>(( a < mx ) * a + (not ( a < mx )) * mx)
    return <float>mn if a < mn else <float>mx if a > mx else <float>a


cdef inline double clamp_int_to_double(int a, int mn=0, int mx=1) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    return <double>mn if a < mn else <double>mx if a > mx else <double>a


cdef inline int clamp_int(int a, int mn=0, int mx=1) nogil:
    """limits the input number a to the range ( mn < a < mx ).
    """
    return mn if a < mn else mx if a > mx else a

cpdef int rect_intersection(tuple r1, tuple r2):
    
    cdef double x0 = r1[0]
    cdef double y0 = r1[1]
    cdef double x1 = r1[0]+r1[2]
    cdef double y1 = r1[1]+r1[3]

    cdef double x2 = r2[0]
    cdef double y2 = r2[1]
    cdef double x3 = r2[0]+r2[2]
    cdef double y3 = r2[1]+r2[3]

    return (( x0 < x3 ) and ( x1 > x2 ) and ( y0 < y2 ) and ( y1 > y2 ))

cdef inline bint rect_intersection_fast(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3):
    return ( x0 < x3 ) * ( x1 > x2 ) * ( y0 < y3 ) * ( y1 > y2 )


ctypedef fused list_or_tuple:
    list
    tuple


cdef struct CONSTANTS:
    double TAU
    double PI
    double PIHALF
    double R2D
    double D2R


cdef CONSTANTS CONST

CONST.PI = pi
CONST.TAU = 2.0*pi
CONST.PIHALF = pi/2.0
CONST.R2D = 180.0/pi
CONST.D2R = pi/180.0

cdef Rect INIT_RECT = pgRect_New4(0,0,0,0)
cdef Rect UNIT_CHARMAP_RECT = pgRect_New4(1,0,1,1)





cdef class AbstractGroup


cdef class Sprite:
    """simple base class for visible game objects

    pygame.sprite.Sprite(*groups): return Sprite

    The base class for visible game objects. Derived classes will want to
    override the Sprite.update() method and assign Sprite.image and Sprite.rect
    attributes.  The initializer can accept any number of Group instances that
    the Sprite will become a member of.

    When subclassing the Sprite class, be sure to call the base initializer
    before adding the Sprite to Groups.

    """

    cdef public dict __g
    cdef object _img
    cdef Image _tex_img
    cdef public Rect rect
    cdef public double distance, _x, _y
    cdef public int _layer
    cdef dict __dict__

    def __cinit__(self):
        self.__dict__ = {}
        self.__g = {} # The groups the sprite is in

    
    def __init__(self, *groups):
        if groups:
            self.add(*groups)


    cpdef _get_img(self):
        return self._img

    cpdef _set_img(self, img):
        if isinstance(img, Image):
            self._tex_img = <Image>img

        self._img = img
    
    image = property(_get_img, _set_img, doc="custom getter and setter for the 'image' variable that supports surfaces and Image objects")

    def add(self, *groups):
        """add the sprite to groups

        Sprite.add(*groups): return None

        Any number of Group instances can be passed as arguments. The
        Sprite will be added to the Groups it is not already a member of.

        """
        has = self.__g.__contains__
        for group in groups:
            if hasattr(group, '_spritegroup'):
                if not has(group):
                    (<AbstractGroup>group).add_internal(self)
                    self.add_internal(<AbstractGroup>group)
            else:
                self.add(*group)

    def remove(self, *groups):
        """remove the sprite from groups

        Sprite.remove(*groups): return None

        Any number of Group instances can be passed as arguments. The Sprite
        will be removed from the Groups it is currently a member of.

        """
        has = self.__g.__contains__
        for group in groups:
            if hasattr(group, '_spritegroup'):
                if has(group):
                    group.remove_internal(self)
                    self.remove_internal(<AbstractGroup>group)
            else:
                self.remove(*group)

    cpdef void add_internal(self, group):
        self.__g[group] = 0

    cpdef void remove_internal(self, group):
        if group in self.__g:
            del self.__g[group]

    def update(self, *args, **kwargs):
        """method to control sprite behavior

        Sprite.update(*args, **kwargs):

        The default implementation of this method does nothing; it's just a
        convenient "hook" that you can override. This method is called by
        Group.update() with whatever arguments you give it.

        There is no need to use this method if not using the convenience
        method by the same name in the Group class.

        """
        pass

    def kill(self):
        """remove the Sprite from all Groups

        Sprite.kill(): return None

        The Sprite is removed from all the Groups that contain it. This won't
        change anything about the state of the Sprite. It is possible to
        continue to use the Sprite after this method has been called, including
        adding it to Groups.

        """
        for c in self.__g:
            c.remove_internal(self)
        self.__g.clear()

    def groups(self):
        """list of Groups that contain this Sprite

        Sprite.groups(): return group_list

        Returns a list of all the Groups that contain this Sprite.

        """
        return list(self.__g)

    def alive(self):
        """does the sprite belong to any groups

        Sprite.alive(): return bool

        Returns True when the Sprite belongs to one or more Groups.
        """
        return truth(self.__g)

    def __repr__(self):
        return "<%s sprite(in %d groups)>" % (self.__class__.__name__, len(self.__g))


class DirtySprite(Sprite):
    """a more featureful subclass of Sprite with more attributes

    pygame.sprite.DirtySprite(*groups): return DirtySprite

    Extra DirtySprite attributes with their default values:

    dirty = 1
        If set to 1, it is repainted and then set to 0 again.
        If set to 2, it is always dirty (repainted each frame;
        flag is not reset).
        If set to 0, it is not dirty and therefore not repainted again.

    blendmode = 0
        It's the special_flags argument of Surface.blit; see the blendmodes in
        the Surface.blit documentation

    source_rect = None
        This is the source rect to use. Remember that it is relative to the top
        left corner (0, 0) of self.image.

    visible = 1
        Normally this is 1. If set to 0, it will not be repainted. (If you
        change visible to 1, you must set dirty to 1 for it to be erased from
        the screen.)

    _layer = 0
        0 is the default value but this is able to be set differently
        when subclassing.

    """

    def __init__(self, *groups):

        self.dirty = 1
        self.blendmode = 0  # pygame 1.8, referred to as special_flags in
                            # the documentation of Surface.blit
        self._visible = 1
        self._layer = getattr(self, '_layer', 0)    # Default 0 unless
                                                    # initialized differently.
        self.source_rect = None
        Sprite.__init__(self, *groups)

    def _set_visible(self, val):
        """set the visible value (0 or 1) and makes the sprite dirty"""
        self._visible = val
        if self.dirty < 2:
            self.dirty = 1

    def _get_visible(self):
        """return the visible value of that sprite"""
        return self._visible

    visible = property(lambda self: self._get_visible(),
                       lambda self, value: self._set_visible(value),
                       doc="you can make this sprite disappear without "
                           "removing it from the group,\n"
                           "assign 0 for invisible and 1 for visible")

    def __repr__(self):
        return "<%s DirtySprite(in %d groups)>" % \
            (self.__class__.__name__, len(self.groups()))


cdef class AbstractGroup:
    """base class for containers of sprites

    AbstractGroup does everything needed to behave as a normal group. You can
    easily subclass a new group class from this or the other groups below if
    you want to add more features.

    Any AbstractGroup-derived sprite groups act like sequences and support
    iteration, len, and so on.

    """

    # dummy val to identify sprite groups, and avoid infinite recursion
    _spritegroup = True

    cdef public dict spritedict
    cdef public list lostsprites

    def __cinit__(self):
        self.spritedict = {}
        self.lostsprites = []

    cpdef list sprites(self):
        """get a list of sprites in the group

        Group.sprite(): return list

        Returns an object that can be looped over with a 'for' loop. (For now,
        it is always a list, but this could change in a future version of
        pygame.) Alternatively, you can get the same information by iterating
        directly over the sprite group, e.g. 'for sprite in group'.

        """
        return list(self.spritedict)

    cpdef void add_internal(self, sprite):
        self.spritedict[sprite] = 0

    cpdef void remove_internal(self, sprite):
        r = self.spritedict[sprite]
        if r:
            self.lostsprites.append(r)
        del self.spritedict[sprite]

    cpdef bint has_internal(self, sprite):
        return sprite in self.spritedict

    def copy(self):
        """copy a group with all the same sprites

        Group.copy(): return Group

        Returns a copy of the group that is an instance of the same class
        and has the same sprites in it.

        """
        return self.__class__(self.sprites())

    def __iter__(self):
        return iter(self.sprites())

    def __contains__(self, sprite):
        return self.has(sprite)

    def add(self, *sprites):
        """add sprite(s) to group

        Group.add(sprite, list, group, ...): return None

        Adds a sprite or sequence of sprites to a group.

        """
        for sprite in sprites:
            # It's possible that some sprite is also an iterator.
            # If this is the case, we should add the sprite itself,
            # and not the iterator object.
            if isinstance(sprite, Sprite):
                if not self.has_internal(<Sprite>sprite):
                    self.add_internal(<Sprite>sprite)
                    (<Sprite>sprite).add_internal(self)
            else:
                try:
                    # See if sprite is an iterator, like a list or sprite
                    # group.
                    self.add(*sprite)
                except (TypeError, AttributeError):
                    # Not iterable. This is probably a sprite that is not an
                    # instance of the Sprite class or is not an instance of a
                    # subclass of the Sprite class. Alternately, it could be an
                    # old-style sprite group.
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if not self.has_internal(spr):
                                self.add_internal(spr)
                                spr.add_internal(self)
                    elif not self.has_internal(sprite):
                        self.add_internal(sprite)
                        sprite.add_internal(self)

    def remove(self, *sprites):
        """remove sprite(s) from group

        Group.remove(sprite, list, or group, ...): return None

        Removes a sprite or sequence of sprites from a group.

        """
        # This function behaves essentially the same as Group.add. It first
        # tries to handle each argument as an instance of the Sprite class. If
        # that failes, then it tries to handle the argument as an iterable
        # object. If that failes, then it tries to handle the argument as an
        # old-style sprite group. Lastly, if that fails, it assumes that the
        # normal Sprite methods should be used.
        for sprite in sprites:
            if isinstance(sprite, Sprite):
                if self.has_internal(<Sprite>sprite):
                    self.remove_internal(<Sprite>sprite)
                    <Sprite>sprite.remove_internal(self)
            else:
                try:
                    self.remove(*sprite)
                except (TypeError, AttributeError):
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if self.has_internal(spr):
                                self.remove_internal(spr)
                                spr.remove_internal(self)
                    elif self.has_internal(sprite):
                        self.remove_internal(sprite)
                        sprite.remove_internal(self)

    def has(self, *sprites):
        """ask if group has a sprite or sprites

        Group.has(sprite or group, ...): return bool

        Returns True if the given sprite or sprites are contained in the
        group. Alternatively, you can get the same information using the
        'in' operator, e.g. 'sprite in group', 'subgroup in group'.

        """
        return_value = False

        for sprite in sprites:
            if isinstance(sprite, Sprite):
                # Check for Sprite instance's membership in this group
                if self.has_internal(<Sprite>sprite):
                    return_value = True
                else:
                    return False
            else:
                try:
                    if self.has(*sprite):
                        return_value = True
                    else:
                        return False
                except (TypeError, AttributeError):
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if self.has_internal(spr):
                                return_value = True
                            else:
                                return False
                    else:
                        if self.has_internal(sprite):
                            return_value = True
                        else:
                            return False

        return return_value

    def update(self, *args, **kwargs):
        """call the update method of every member sprite

        Group.update(*args, **kwargs): return None

        Calls the update method of every member sprite. All arguments that
        were passed to this method are passed to the Sprite update function.

        """
        for s in self.sprites():
            s.update(*args, **kwargs)

    def draw(self, surface):
        """draw all sprites onto the surface

        Group.draw(surface): return None

        Draws all of the member sprites onto the given surface.

        """
        cdef list sprites = self.sprites()
        cdef object surface_blit
        cdef spritedict = self.spritedict
        cdef object ret

        if isinstance(surface, Renderer):
            for spr in sprites:
                ret = (<Renderer>surface).blit(spr.image, spr.rect)
                PyDict_SetItem(spritedict, spr, ret)
        else:
            surface_blit = surface.blit
            for spr in sprites:
                ret = PyObject_CallFunctionObjArgs(surface_blit,
                                                   <PyObject*>spr.image,
                                                   <PyObject*>spr.rect, NULL)
                PyDict_SetItem(spritedict, spr, ret)

        self.lostsprites*=0

    def clear(self, surface, bgd):
        """erase the previous position of all sprites

        Group.clear(surface, bgd): return None

        Clears the area under every drawn sprite in the group. The bgd
        argument should be Surface which is the same dimensions as the
        screen surface. The bgd could also be a function which accepts
        the given surface and the area to be cleared as arguments.

        """
        if callable(bgd):
            for r in self.lostsprites:
                bgd(surface, r)
            for r in self.spritedict.values():
                if r:
                    bgd(surface, r)
        else:
            surface_blit = surface.blit
            for r in self.lostsprites:
                surface_blit(bgd, r, r)
            for r in self.spritedict.values():
                if r:
                    surface_blit(bgd, r, r)

    def empty(self):
        """remove all sprites

        Group.empty(): return None

        Removes all the sprites from the group.

        """
        for s in self.sprites():
            self.remove_internal(<Sprite>s)
            s.remove_internal(self)

    def __nonzero__(self):
        return truth(self.sprites())

    def __len__(self):
        """return number of sprites in group

        Group.len(group): return int

        Returns the number of sprites contained in the group.

        """
        return len(self.sprites())

    def __repr__(self):
        return "<%s(%d sprites)>" % (self.__class__.__name__, len(self))

cdef class Group(AbstractGroup):
    """container class for many Sprites

    pygame.sprite.Group(*sprites): return Group

    A simple container for Sprite objects. This class can be subclassed to
    create containers with more specific behaviors. The constructor takes any
    number of Sprite arguments to add to the Group. The group supports the
    following standard Python operations:

        in      test if a Sprite is contained
        len     the number of Sprites contained
        bool    test if any Sprites are contained
        iter    iterate through all the Sprites

    The Sprites in the Group are not ordered, so the Sprites are drawn and
    iterated over in no particular order.

    """
    def __init__(self, *sprites):
        AbstractGroup.__init__(self)
        self.add(*sprites)

RenderPlain = Group
RenderClear = Group

cdef class RenderUpdates(Group):
    """Group class that tracks dirty updates

    pygame.sprite.RenderUpdates(*sprites): return RenderUpdates

    This class is derived from pygame.sprite.Group(). It has an enhanced draw
    method that tracks the changed areas of the screen.

    """
    def draw(self, surface):
       spritedict = self.spritedict
       surface_blit = surface.blit
       dirty = self.lostsprites
       self.lostsprites.clear()
       dirty_append = dirty.append

       if isinstance(surface, Renderer):
            
            for s in self.sprites():
                r = spritedict[s]
                newrect = (<Renderer>surface).blit(s.image, s.rect)
                if r:
                    if newrect.colliderect(r):
                        dirty_append(newrect.union(r))
                    else:
                        dirty_append(newrect)
                        dirty_append(r)
                    
                PyDict_SetItem(spritedict, s, newrect)
            
            return dirty

        
       for s in self.sprites():
           r = spritedict[s]
           newrect = surface_blit(s.image, s.rect)
           if r:
               if newrect.colliderect(r):
                   dirty_append(newrect.union(r))
               else:
                   dirty_append(newrect)
                   dirty_append(r)
           else:
               dirty_append(newrect)
            
           PyDict_SetItem(spritedict, s, newrect)

       return dirty

cdef class OrderedUpdates(RenderUpdates):
    """RenderUpdates class that draws Sprites in order of addition

    pygame.sprite.OrderedUpdates(*spites): return OrderedUpdates

    This class derives from pygame.sprite.RenderUpdates().  It maintains
    the order in which the Sprites were added to the Group for rendering.
    This makes adding and removing Sprites from the Group a little
    slower than regular Groups.

    """

    cdef public list _spritelist

    def __init__(self, *sprites):
        self._spritelist = []
        RenderUpdates.__init__(self, *sprites)

    cpdef list sprites(self):
        return list(self._spritelist)

    cpdef void add_internal(self, sprite):
        RenderUpdates.add_internal(self, sprite)
        self._spritelist.append(sprite)

    cpdef void remove_internal(self, sprite):
        RenderUpdates.remove_internal(self, sprite)
        self._spritelist.remove(sprite)


cdef class LayeredUpdates(AbstractGroup):
    """LayeredUpdates Group handles layers, which are drawn like OrderedUpdates

    pygame.sprite.LayeredUpdates(*spites, **kwargs): return LayeredUpdates

    This group is fully compatible with pygame.sprite.Sprite.
    New in pygame 1.8.0

    """

    _init_rect = Rect(0, 0, 0, 0)

    cdef public dict _spritelayers
    cdef public list _spritelist
    cdef public int _default_layer

    def __cinit__(self):
        self._spritelayers = {}
        self._spritelist = []

    def __init__(self, *sprites, **kwargs):
        """initialize an instance of LayeredUpdates with the given attributes

        You can set the default layer through kwargs using 'default_layer'
        and an integer for the layer. The default layer is 0.

        If the sprite you add has an attribute _layer, then that layer will be
        used. If **kwarg contains 'layer', then the passed sprites will be
        added to that layer (overriding the sprite._layer attribute). If
        neither the sprite nor **kwarg has a 'layer', then the default layer is
        used to add the sprites.

        """
        AbstractGroup.__init__(self)
        self._default_layer = kwargs.get('default_layer', 0)

        self.add(*sprites, **kwargs)

    cpdef void add_internal(self, sprite, layer=None):
        """Do not use this method directly.

        It is used by the group to add a sprite internally.

        """
        self.spritedict[sprite] = self._init_rect

        if layer is None:
            try:
                layer = sprite._layer
            except AttributeError:
                layer = sprite._layer = self._default_layer
        elif hasattr(sprite, '_layer'):
            sprite._layer = layer

        cdef list sprites = self._spritelist # speedup
        cdef dict sprites_layers = self._spritelayers
        sprites_layers[sprite] = layer

        # add the sprite at the right position
        # bisect algorithmus
        cdef int low, med, high
        cdef int leng = len(sprites)
        low = mid = 0
        high = leng - 1
        while low <= high:
            mid = low + (high - low) // 2
            if sprites_layers[sprites[mid]] <= layer:
                low = mid + 1
            else:
                high = mid - 1
        # linear search to find final position
        while mid < leng and sprites_layers[sprites[mid]] <= layer:
            mid += 1
        sprites.insert(mid, sprite)

    def add(self, *sprites, **kwargs):
        """add a sprite or sequence of sprites to a group

        LayeredUpdates.add(*sprites, **kwargs): return None

        If the sprite you add has an attribute _layer, then that layer will be
        used. If **kwarg contains 'layer', then the passed sprites will be
        added to that layer (overriding the sprite._layer attribute). If
        neither the sprite nor **kwarg has a 'layer', then the default layer is
        used to add the sprites.

        """

        if not sprites:
            return
        if 'layer' in kwargs:
            layer = kwargs['layer']
        else:
            layer = None

        cdef Sprite csprite
        for sprite in sprites:
            # It's possible that some sprite is also an iterator.
            # If this is the case, we should add the sprite itself,
            # and not the iterator object.
            if isinstance(sprite, Sprite):
                if not self.has_internal(<Sprite>sprite):
                    self.add_internal(<Sprite>sprite, layer)
                    (<Sprite>sprite).add_internal(self)
            else:
                try:
                    # See if sprite is an iterator, like a list or sprite
                    # group.
                    self.add(*sprite, **kwargs)
                except (TypeError, AttributeError):
                    # Not iterable. This is probably a sprite that is not an
                    # instance of the Sprite class or is not an instance of a
                    # subclass of the Sprite class. Alternately, it could be an
                    # old-style sprite group.
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if not self.has_internal(spr):
                                self.add_internal(spr, layer)
                                spr.add_internal(self)
                    elif not self.has_internal(sprite):
                        self.add_internal(sprite, layer)
                        sprite.add_internal(self)

    cpdef void remove_internal(self, sprite):
        """Do not use this method directly.

        The group uses it to add a sprite.

        """
        self._spritelist.remove(sprite)
        # these dirty rects are suboptimal for one frame
        cdef Rect r = self.spritedict[sprite]
        if r is not self._init_rect:
            self.lostsprites.append(r) # dirty rect
        if hasattr(sprite, 'rect'):
            self.lostsprites.append(sprite.rect) # dirty rect

        del self.spritedict[sprite]
        del self._spritelayers[sprite]

    cpdef list sprites(self):
        """return a ordered list of sprites (first back, last top).

        LayeredUpdates.sprites(): return sprites

        """
        return list(self._spritelist)

    def draw(self, surface):
        """draw all sprites in the right order onto the passed surface

        LayeredUpdates.draw(surface): return Rect_list

        """
        cdef list sprites = self.sprites()
        cdef dict spritedict = self.spritedict
        cdef Rect rec, newrect
        surface_blit = surface.blit
        dirty = self.lostsprites
        self.lostsprites*=0
        dirty_append = dirty.append
        init_rect = self._init_rect

        if isinstance(surface, Renderer):
            
            for spr in sprites:
                rec = spritedict[spr]
                newrect = (<Renderer>surface).blit(spr.image, spr.rect)
                if newrect is init_rect:
                    dirty_append(newrect)
                else:
                    if newrect.colliderect(rec):
                        dirty_append(newrect.union(rec))
                    else:
                        dirty_append(newrect)
                    
                PyDict_SetItem(spritedict, spr, newrect)
            
            return dirty

        else:

            for spr in sprites:
                rec = spritedict[spr]
                
                newrect = PyObject_CallFunctionObjArgs(surface_blit, <PyObject*>spr.image, <PyObject*>spr.rect, NULL)

                if rec is init_rect:
                    dirty_append(newrect)
                else:
                    if newrect.colliderect(rec):
                        dirty_append(newrect.union(rec))
                    else:
                        dirty_append(newrect)
                        dirty_append(rec)
                    
                PyDict_SetItem(spritedict, spr, newrect)
            
            return dirty

    def get_sprites_at(self, pos):
        """return a list with all sprites at that position

        LayeredUpdates.get_sprites_at(pos): return colliding_sprites

        Bottom sprites are listed first; the top ones are listed last.

        """
        cdef Py_ssize_t i
        cdef list _sprites = self._spritelist
        cdef Rect rect = Rect(pos, (0, 0))
        cdef list colliding_idx = rect.collidelistall(_sprites)
        cdef list colliding = []

        for i in range(len(colliding_idx)):
            colliding.append(_sprites[i])

        return colliding

    def get_sprite(self, idx):
        """return the sprite at the index idx from the groups sprites

        LayeredUpdates.get_sprite(idx): return sprite

        Raises IndexOutOfBounds if the idx is not within range.

        """
        return self._spritelist[idx]

    def remove_sprites_of_layer(self, layer_nr):
        """remove all sprites from a layer and return them as a list

        LayeredUpdates.remove_sprites_of_layer(layer_nr): return sprites

        """
        sprites = self.get_sprites_from_layer(layer_nr)
        self.remove(*sprites)
        return sprites

    #---# layer methods
    cpdef list layers(self):
        """return a list of unique defined layers defined.

        LayeredUpdates.layers(): return layers

        """
        return sorted(set(self._spritelayers.values()))

    def change_layer(self, sprite, int new_layer):
        """change the layer of the sprite

        LayeredUpdates.change_layer(sprite, new_layer): return None

        The sprite must have been added to the renderer already. This is not
        checked.

        """
        sprites = self._spritelist # speedup
        sprites_layers = self._spritelayers # speedup

        sprites.remove(sprite)
        sprites_layers.pop(sprite)

        # add the sprite at the right position
        # bisect algorithmus
        cdef int low, mid, high
        cdef int leng = len(sprites)
        low = mid = 0
        high = leng - 1
        cdef int layer1

        while low <= high:
            mid = low + (high - low) // 2
            if sprites_layers[sprites[mid]] <= new_layer:
                low = mid + 1
            else:
                high = mid - 1
        # linear search to find final position
        while mid < leng and sprites_layers[sprites[mid]] <= new_layer:
            mid += 1
        
        sprites.insert(mid, sprite)


        sprites.insert(mid, sprite)
        if hasattr(sprite, 'layer'):
            sprite.layer = new_layer

        # add layer info
        sprites_layers[sprite] = new_layer

    def get_layer_of_sprite(self, sprite):
        """return the layer that sprite is currently in

        If the sprite is not found, then it will return the default layer.

        """
        return self._spritelayers.get(sprite, self._default_layer)

    def get_top_layer(self):
        """return the top layer

        LayeredUpdates.get_top_layer(): return layer

        """
        return self._spritelayers[self._spritelist[-1]]

    def get_bottom_layer(self):
        """return the bottom layer

        LayeredUpdates.get_bottom_layer(): return layer

        """
        return self._spritelayers[self._spritelist[0]]

    def move_to_front(self, sprite):
        """bring the sprite to front layer

        LayeredUpdates.move_to_front(sprite): return None

        Brings the sprite to front by changing the sprite layer to the top-most
        layer. The sprite is added at the end of the list of sprites in that
        top-most layer.

        """
        self.change_layer(sprite, self.get_top_layer())

    def move_to_back(self, sprite):
        """move the sprite to the bottom layer

        LayeredUpdates.move_to_back(sprite): return None

        Moves the sprite to the bottom layer by moving it to a new layer below
        the current bottom layer.

        """
        self.change_layer(sprite, self.get_bottom_layer() - 1)

    def get_top_sprite(self):
        """return the topmost sprite

        LayeredUpdates.get_top_sprite(): return Sprite

        """
        return self._spritelist[-1]

    def get_sprites_from_layer(self, int layer):
        """return all sprites from a layer ordered as they where added

        LayeredUpdates.get_sprites_from_layer(layer): return sprites

        Returns all sprites from a layer. The sprites are ordered in the
        sequence that they where added. (The sprites are not removed from the
        layer.

        """
        cdef Py_ssize_t i
        cdef list sprites = []
        cdef dict sprite_layers = self._spritelayers
        cdef list spritelist = self._spritelist
        cdef int l = len(spritelist)

        cdef int layer2



        for i in range(l):
            spr = spritelist[i]
            layer2 = sprite_layers[spr]
            if layer2 == layer:
                sprites.append(spr)
            elif layer2 > layer:# break after because no other will
                                            # follow with same layer
                break
        return sprites

    def switch_layer(self, int layer1_nr, int layer2_nr):
        """switch the sprites from layer1_nr to layer2_nr

        LayeredUpdates.switch_layer(layer1_nr, layer2_nr): return None

        The layers number must exist. This method does not check for the
        existence of the given layers.

        """
        cdef Py_ssize_t i
        cdef list sprites1 = self.remove_sprites_of_layer(layer1_nr)
       
        cdef list sprites_from_layer2 = self.get_sprites_from_layer(layer2_nr)
        l = len(sprites_from_layer2)
        for i in range(l):
            spr = sprites_from_layer2[i]
            self.change_layer(spr, layer1_nr)
        self.add(layer=layer2_nr, *sprites1)


cdef class LayeredDirty(LayeredUpdates):
    """LayeredDirty Group is for DirtySprites; subclasses LayeredUpdates

    pygame.sprite.LayeredDirty(*spites, **kwargs): return LayeredDirty

    This group requires pygame.sprite.DirtySprite or any sprite that
    has the following attributes:
        image, rect, dirty, visible, blendmode (see doc of DirtySprite).

    It uses the dirty flag technique and is therefore faster than
    pygame.sprite.RenderUpdates if you have many static sprites.  It
    also switches automatically between dirty rect updating and full
    screen drawing, so you do no have to worry which would be faster.

    As with the pygame.sprite.Group, you can specify some additional attributes
    through kwargs:
        _use_update: True/False   (default is False)
        _default_layer: default layer where the sprites without a layer are
            added
        _time_threshold: treshold time for switching between dirty rect mode
            and fullscreen mode; defaults to updating at 80 frames per second,
            which is equal to 1000.0 / 80.0

    New in pygame 1.8.0

    """

    cdef public Rect _clip
    cdef public bint _use_update
    cdef public float _time_threshold
    cdef public object _bgd

    def __init__(self, *sprites, **kwargs):
        """initialize group.

        pygame.sprite.LayeredDirty(*spites, **kwargs): return LayeredDirty

        You can specify some additional attributes through kwargs:
            _use_update: True/False   (default is False)
            _default_layer: default layer where the sprites without a layer are
                added
            _time_threshold: treshold time for switching between dirty rect
                mode and fullscreen mode; defaults to updating at 80 frames per
                second, which is equal to 1000.0 / 80.0

        """
        LayeredUpdates.__init__(self, *sprites, **kwargs)
        self._clip = None

        self._use_update = False

        self._time_threshold = 1000.0 / 80.0 # 1000.0 / fps

        self._bgd = None
        for key, val in kwargs.items():
            if key in ['_use_update', '_time_threshold', '_default_layer']:
                if hasattr(self, key):
                    setattr(self, key, val)

    cpdef void add_internal(self, sprite, layer=None):
        """Do not use this method directly.

        It is used by the group to add a sprite internally.

        """
        # check if all needed attributes are set
        if not hasattr(sprite, 'dirty'):
            raise AttributeError()
        if not hasattr(sprite, 'visible'):
            raise AttributeError()
        if not hasattr(sprite, 'blendmode'):
            raise AttributeError()

        if not isinstance(sprite, DirtySprite):
            raise TypeError()

        if sprite.dirty == 0: # set it dirty if it is not
            sprite.dirty = 1

        LayeredUpdates.add_internal(self, sprite, layer)

    def draw(self, surface, bgd=None):
        """draw all sprites in the right order onto the given surface

        LayeredDirty.draw(surface, bgd=None): return Rect_list

        You can pass the background too. If a self.bgd is already set to some
        value that is not None, then the bgd argument has no effect.

        """
        # speedups
        _orig_clip = surface.get_clip()
        _clip = self._clip
        if _clip is None:
            _clip = _orig_clip

        _surf = surface
        _sprites = self._spritelist
        _old_rect = self.spritedict
        _update = self.lostsprites
        _update_append = _update.append
        _ret = None
        _surf_blit = _surf.blit
        _rect = Rect
        if bgd is not None:
            self._bgd = bgd
        _bgd = self._bgd
        init_rect = self._init_rect

        _surf.set_clip(_clip)
        # -------
        # 0. decide whether to render with update or flip
        start_time = get_ticks()
        if self._use_update: # dirty rects mode
            # 1. find dirty area on screen and put the rects into _update
            # still not happy with that part
            for spr in _sprites:
                if 0 < spr.dirty:
                    # chose the right rect
                    if spr.source_rect:
                        _union_rect = _rect(spr.rect.topleft,
                                            spr.source_rect.size)
                    else:
                        _union_rect = _rect(spr.rect)

                    _union_rect_collidelist = _union_rect.collidelist
                    _union_rect_union_ip = _union_rect.union_ip
                    i = _union_rect_collidelist(_update)
                    while -1 < i:
                        _union_rect_union_ip(_update[i])
                        del _update[i]
                        i = _union_rect_collidelist(_update)
                    _update_append(_union_rect.clip(_clip))

                    if _old_rect[spr] is not init_rect:
                        _union_rect = _rect(_old_rect[spr])
                        _union_rect_collidelist = _union_rect.collidelist
                        _union_rect_union_ip = _union_rect.union_ip
                        i = _union_rect_collidelist(_update)
                        while -1 < i:
                            _union_rect_union_ip(_update[i])
                            del _update[i]
                            i = _union_rect_collidelist(_update)
                        _update_append(_union_rect.clip(_clip))
            # can it be done better? because that is an O(n**2) algorithm in
            # worst case

            # clear using background
            if _bgd is not None:
                for rec in _update:
                    _surf_blit(_bgd, rec, rec)

            # 2. draw
            for spr in _sprites:
                if 1 > spr.dirty:
                    if spr._visible:
                        # sprite not dirty; blit only the intersecting part
                        if spr.source_rect is not None:
                            # For possible future speed up, source_rect's data
                            # can be prefetched outside of this loop.
                            _spr_rect = _rect(spr.rect.topleft,
                                              spr.source_rect.size)
                            rect_offset_x = spr.source_rect[0] - _spr_rect[0]
                            rect_offset_y = spr.source_rect[1] - _spr_rect[1]
                        else:
                            _spr_rect = spr.rect
                            rect_offset_x = -_spr_rect[0]
                            rect_offset_y = -_spr_rect[1]

                        _spr_rect_clip = _spr_rect.clip

                        for idx in _spr_rect.collidelistall(_update):
                            # clip
                            clip = _spr_rect_clip(_update[idx])
                            _surf_blit(spr.image,
                                       clip,
                                       (clip[0] + rect_offset_x,
                                        clip[1] + rect_offset_y,
                                        clip[2],
                                        clip[3]),
                                       spr.blendmode)
                else: # dirty sprite
                    if spr._visible:
                        _old_rect[spr] = _surf_blit(spr.image,
                                                    spr.rect,
                                                    spr.source_rect,
                                                    spr.blendmode)
                    if spr.dirty == 1:
                        spr.dirty = 0
            _ret = list(_update)
        else: # flip, full screen mode
            if _bgd is not None:
                _surf_blit(_bgd, (0, 0))
            for spr in _sprites:
                if spr._visible:
                    _old_rect[spr] = _surf_blit(spr.image,
                                                spr.rect,
                                                spr.source_rect,
                                                spr.blendmode)
            _ret = [_rect(_clip)] # return only the part of the screen changed


        # timing for switching modes
        # How may a good threshold be found? It depends on the hardware.
        end_time = get_ticks()
        if end_time-start_time > self._time_threshold:
            self._use_update = False
        else:
            self._use_update = True

##        # debug
##        print "               check: using dirty rects:", self._use_update

        # emtpy dirty rects list
        _update[:] = []

        # -------
        # restore original clip
        _surf.set_clip(_orig_clip)
        return _ret

    def clear(self, surface, bgd):
        """use to set background

        Group.clear(surface, bgd): return None

        """
        self._bgd = bgd

    def repaint_rect(self, screen_rect):
        """repaint the given area

        LayeredDirty.repaint_rect(screen_rect): return None

        screen_rect is in screen coordinates.

        """
        if self._clip:
            self.lostsprites.append(screen_rect.clip(self._clip))
        else:
            self.lostsprites.append(Rect(screen_rect))

    def set_clip(self, screen_rect=None):
        """clip the area where to draw; pass None (default) to reset the clip

        LayeredDirty.set_clip(screen_rect=None): return None

        """
        if screen_rect is None:
            self._clip = pygame.display.get_surface().get_rect()
        else:
            self._clip = screen_rect
        self._use_update = False

    def get_clip(self):
        """get the area where drawing will occur

        LayeredDirty.get_clip(): return Rect

        """
        return self._clip

    def change_layer(self, sprite, new_layer):
        """change the layer of the sprite

        LayeredUpdates.change_layer(sprite, new_layer): return None

        The sprite must have been added to the renderer already. This is not
        checked.

        """
        LayeredUpdates.change_layer(self, sprite, new_layer)
        if sprite.dirty == 0:
            sprite.dirty = 1

    def set_timing_treshold(self, time_ms):
        """set the treshold in milliseconds

        set_timing_treshold(time_ms): return None

        Defaults to 1000.0 / 80.0. This means that the screen will be painted
        using the flip method rather than the update method if the update
        method is taking so long to update the screen that the frame rate falls
        below 80 frames per second.

        """
        self._time_threshold = time_ms


cdef class GroupSingle(AbstractGroup):
    """A group container that holds a single most recent item.

    This class works just like a regular group, but it only keeps a single
    sprite in the group. Whatever sprite has been added to the group last will
    be the only sprite in the group.

    You can access its one sprite as the .sprite attribute.  Assigning to this
    attribute will properly remove the old sprite and then add the new one.

    """

    cdef public object __sprite

    def __init__(self, sprite=None):
        AbstractGroup.__init__(self)
        self.__sprite = None
        if sprite is not None:
            self.add(sprite)

    def copy(self):
        return GroupSingle(self.__sprite)

    cpdef list sprites(self):
        if self.__sprite is not None:
            return [self.__sprite]
        else:
            return []

    cpdef void add_internal(self, sprite):
        if self.__sprite is not None:
            self.__sprite.remove_internal(self)
            self.remove_internal(<Sprite>self.__sprite)
        self.__sprite = sprite

    def __nonzero__(self):
        return self.__sprite is not None

    def _get_sprite(self):
        return self.__sprite

    def _set_sprite(self, sprite):
        self.add_internal(sprite)
        sprite.add_internal(self)
        return sprite

    sprite = property(_get_sprite,
                      _set_sprite,
                      None,
                      "The sprite contained in this group")

    cpdef void remove_internal(self, sprite):
        if sprite is self.__sprite:
            self.__sprite = None
        if sprite in self.spritedict:
            AbstractGroup.remove_internal(self, sprite)

    cpdef bint has_internal(self, sprite):
        return self.__sprite is sprite

    # Optimizations...
    def __contains__(self, sprite):
        return self.__sprite is sprite


# Some different collision detection functions that could be used.
def collide_rect(left, right):
    """collision detection between two sprites, using rects.

    pygame.sprite.collide_rect(left, right): return bool

    Tests for collision between two sprites. Uses the pygame.Rect colliderect
    function to calculate the collision. It is intended to be passed as a
    collided callback function to the *collide functions. Sprites must have
    "rect" attributes.

    New in pygame 1.8.0

    """
    return left.rect.colliderect(right.rect)

class collide_rect_ratio:
    """A callable class that checks for collisions using scaled rects

    The class checks for collisions between two sprites using a scaled version
    of the sprites' rects. Is created with a ratio; the instance is then
    intended to be passed as a collided callback function to the *collide
    functions.

    New in pygame 1.8.1

    """

    def __init__(self, ratio):
        """create a new collide_rect_ratio callable

        Ratio is expected to be a floating point value used to scale
        the underlying sprite rect before checking for collisions.

        """
        self.ratio = ratio

    def __call__(self, left, right):
        """detect collision between two sprites using scaled rects

        pygame.sprite.collide_rect_ratio(ratio)(left, right): return bool

        Tests for collision between two sprites. Uses the pygame.Rect
        colliderect function to calculate the collision after scaling the rects
        by the stored ratio. Sprites must have "rect" attributes.

        """

        ratio = self.ratio

        leftrect = left.rect
        width = leftrect.width
        height = leftrect.height
        leftrect = leftrect.inflate(width * ratio - width,
                                    height * ratio - height)

        rightrect = right.rect
        width = rightrect.width
        height = rightrect.height
        rightrect = rightrect.inflate(width * ratio - width,
                                      height * ratio - height)

        return leftrect.colliderect(rightrect)

def collide_circle(left, right):
    """detect collision between two sprites using circles

    pygame.sprite.collide_circle(left, right): return bool

    Tests for collision between two sprites by testing whether two circles
    centered on the sprites overlap. If the sprites have a "radius" attribute,
    then that radius is used to create the circle; otherwise, a circle is
    created that is big enough to completely enclose the sprite's rect as
    given by the "rect" attribute. This function is intended to be passed as
    a collided callback function to the *collide functions. Sprites must have a
    "rect" and an optional "radius" attribute.

    New in pygame 1.8.0

    """

    xdistance = left.rect.centerx - right.rect.centerx
    ydistance = left.rect.centery - right.rect.centery
    distancesquared = xdistance ** 2 + ydistance ** 2

    if hasattr(left, 'radius'):
        leftradius = left.radius
    else:
        leftrect = left.rect
        # approximating the radius of a square by using half of the diagonal,
        # might give false positives (especially if its a long small rect)
        leftradius = 0.5 * ((leftrect.width ** 2 + leftrect.height ** 2) ** 0.5)
        # store the radius on the sprite for next time
        setattr(left, 'radius', leftradius)

    if hasattr(right, 'radius'):
        rightradius = right.radius
    else:
        rightrect = right.rect
        # approximating the radius of a square by using half of the diagonal
        # might give false positives (especially if its a long small rect)
        rightradius = 0.5 * ((rightrect.width ** 2 + rightrect.height ** 2) ** 0.5)
        # store the radius on the sprite for next time
        setattr(right, 'radius', rightradius)
    return distancesquared <= (leftradius + rightradius) ** 2

class collide_circle_ratio(object):
    """detect collision between two sprites using scaled circles

    This callable class checks for collisions between two sprites using a
    scaled version of a sprite's radius. It is created with a ratio as the
    argument to the constructor. The instance is then intended to be passed as
    a collided callback function to the *collide functions.

    New in pygame 1.8.1

    """

    def __init__(self, ratio):
        """creates a new collide_circle_ratio callable instance

        The given ratio is expected to be a floating point value used to scale
        the underlying sprite radius before checking for collisions.

        When the ratio is ratio=1.0, then it behaves exactly like the 
        collide_circle method.

        """
        self.ratio = ratio


    def __call__(self, left, right):
        """detect collision between two sprites using scaled circles

        pygame.sprite.collide_circle_radio(ratio)(left, right): return bool

        Tests for collision between two sprites by testing whether two circles
        centered on the sprites overlap after scaling the circle's radius by
        the stored ratio. If the sprites have a "radius" attribute, that is
        used to create the circle; otherwise, a circle is created that is big
        enough to completely enclose the sprite's rect as given by the "rect"
        attribute. Intended to be passed as a collided callback function to the
        *collide functions. Sprites must have a "rect" and an optional "radius"
        attribute.

        """

        ratio = self.ratio
        xdistance = left.rect.centerx - right.rect.centerx
        ydistance = left.rect.centery - right.rect.centery
        distancesquared = xdistance ** 2 + ydistance ** 2

        if hasattr(left, "radius"):
            leftradius = left.radius * ratio
        else:
            leftrect = left.rect
            leftradius = ratio * 0.5 * ((leftrect.width ** 2 + leftrect.height ** 2) ** 0.5)
            # store the radius on the sprite for next time
            setattr(left, 'radius', leftradius)

        if hasattr(right, "radius"):
            rightradius = right.radius * ratio
        else:
            rightrect = right.rect
            rightradius = ratio * 0.5 * ((rightrect.width ** 2 + rightrect.height ** 2) ** 0.5)
            # store the radius on the sprite for next time
            setattr(right, 'radius', rightradius)

        return distancesquared <= (leftradius + rightradius) ** 2

def collide_mask(left, right):
    """collision detection between two sprites, using masks.

    pygame.sprite.collide_mask(SpriteLeft, SpriteRight): bool

    Tests for collision between two sprites by testing if their bitmasks
    overlap. If the sprites have a "mask" attribute, that is used as the mask;
    otherwise, a mask is created from the sprite image. Intended to be passed
    as a collided callback function to the *collide functions. Sprites must
    have a "rect" and an optional "mask" attribute.

    New in pygame 1.8.0

    """
    xoffset = right.rect[0] - left.rect[0]
    yoffset = right.rect[1] - left.rect[1]
    try:
        leftmask = left.mask
    except AttributeError:
        leftmask = from_surface(left.image)
    try:
        rightmask = right.mask
    except AttributeError:
        rightmask = from_surface(right.image)
    return leftmask.overlap(rightmask, (xoffset, yoffset))

def spritecollide(sprite, group, dokill, collided=None):
    """find Sprites in a Group that intersect another Sprite

    pygame.sprite.spritecollide(sprite, group, dokill, collided=None):
        return Sprite_list

    Return a list containing all Sprites in a Group that intersect with another
    Sprite. Intersection is determined by comparing the Sprite.rect attribute
    of each Sprite.

    The dokill argument is a bool. If set to True, all Sprites that collide
    will be removed from the Group.

    The collided argument is a callback function used to calculate if two
    sprites are colliding. it should take two sprites as values, and return a
    bool value indicating if they are colliding. If collided is not passed, all
    sprites must have a "rect" value, which is a rectangle of the sprite area,
    which will be used to calculate the collision.

    """
    if dokill:

        crashed = []
        append = crashed.append

        if collided:
            for s in group.sprites():
                if collided(sprite, s):
                    s.kill()
                    append(s)
        else:
            spritecollide = sprite.rect.colliderect
            for s in group.sprites():
                if spritecollide(s.rect):
                    s.kill()
                    append(s)

        return crashed

    elif collided:
        return [s for s in group if collided(sprite, s)]
    else:
        spritecollide = sprite.rect.colliderect
        return [s for s in group if spritecollide(s.rect)]


def groupcollide(groupa, groupb, dokilla, dokillb, collided=None):
    """detect collision between a group and another group

    pygame.sprite.groupcollide(groupa, groupb, dokilla, dokillb):
        return dict

    Given two groups, this will find the intersections between all sprites in
    each group. It returns a dictionary of all sprites in the first group that
    collide. The value for each item in the dictionary is a list of the sprites
    in the second group it collides with. The two dokill arguments control if
    the sprites from either group will be automatically removed from all
    groups. Collided is a callback function used to calculate if two sprites
    are colliding. it should take two sprites as values, and return a bool
    value indicating if they are colliding. If collided is not passed, all
    sprites must have a "rect" value, which is a rectangle of the sprite area
    that will be used to calculate the collision.

    """
    crashed = {}
    SC = spritecollide
    if dokilla:
        for s in groupa.sprites():
            c = SC(s, groupb, dokillb, collided)
            if c:
                crashed[s] = c
                s.kill()
    else:
        for s in groupa:
            c = SC(s, groupb, dokillb, collided)
            if c:
                crashed[s] = c
    return crashed

def spritecollideany(sprite, group, collided=None):
    """finds any sprites in a group that collide with the given sprite

    pygame.sprite.spritecollideany(sprite, group): return sprite

    Given a sprite and a group of sprites, this will return return any single
    sprite that collides with with the given sprite. If there are no
    collisions, then this returns None.

    If you don't need all the features of the spritecollide function, this
    function will be a bit quicker.

    Collided is a callback function used to calculate if two sprites are
    colliding. It should take two sprites as values and return a bool value
    indicating if they are colliding. If collided is not passed, then all
    sprites must have a "rect" value, which is a rectangle of the sprite area,
    which will be used to calculate the collision.


    """
    if collided:
        for s in group:
            if collided(sprite, s):
                return s
    else:
        # Special case old behaviour for speed.
        spritecollide = sprite.rect.colliderect
        for s in group:
            if spritecollide(s.rect):
                return s
    return None



cpdef object cython_fastdraw(object cameraview, Camera2 camera_object, object surface, int parallax=1, int return_dirty_rects=1):

    """Custom function for drawing many sprites at a time, using software blitting.
     
        :param object cameraview: the spritegroup used by a Camera2 object.
        :param Camera2 camera_object: a Camera2 object
        :param object surface: a pygame surface object
        :param int parallax: use per_object parallax

        :param int return_dirty_rects: return dirty rectangles (if necessary)
        
    """

    cdef object dirt_rects, spr, set_clip, colliderect, _union, dirty_append, append

    cdef Rect screenrect, spr_rect, init_rect
    
    cdef Py_ssize_t i

    cdef double VIEW_X, VIEW_Y, parallax_ratio, spr_distance, w, h, srect_x0, srect_y0, srect_x1, srect_y1, spr_x, spr_y, blitpos_x, blitpos_y, t0, dt

    cdef double offset_X = 0.0 
    cdef double offset_Y = 0.0
    cdef list spritelist = <list>cameraview._spritelist  
    cdef dict spritedict = <dict>cameraview.spritedict
    cdef tuple blitpos
    cdef list blit_list = []
        
    set_clip = surface.set_clip
            
    screenrect = camera_object._screenrect
    srect_x0 = screenrect[0]
    srect_y0 = screenrect[1]
    srect_x1 = srect_x0+(screenrect[2])
    srect_y1 = srect_y0+(screenrect[3])
    
    colliderect = Rect.colliderect    # speedup
                
    _union = Rect.union                 # speedup

    cdef list dirty = cameraview.lostsprites
    cameraview.lostsprites = []
    dirty_append = dirty.append
    
    init_rect = cameraview._init_rect

    cdef double X = camera_object._x   # float Camera2 world-coordinates
    cdef double Y = camera_object._y
    
    VIEW_X = screenrect.x    # integer Camera2 screen coordinates (used as an offset for blitting)
    VIEW_Y = screenrect.y

    cdef double view_distance = camera_object._view_distance       # Camera2 maximum view distance
    cdef double last_distace = 0.0

    set_clip(screenrect)

    cdef int spr_list_len = len(spritelist)

    if parallax:
        for i in range(spr_list_len):

            spr = spritelist[i]
            spr_rect = spr.rect
            spr_x = spr_rect.r.x
            spr_y = spr_rect.r.y
            w = spr_rect.r.w
            h = spr_rect.r.h

            spr_distance = spr.distance
            

            if spr_distance != last_distace:                                     
                parallax_ratio = 1.0-clamp_double(fabs(spr_distance/view_distance))     # percentage to multiply sprite displacement by, the higher the sprite distance, the lower the displacement.
            
                offset_X = parallax_ratio*X
                offset_Y = parallax_ratio*Y

                last_distace = spr_distance

            blitpos_x = VIEW_X+clamp_double(spr_x-offset_X, SHRT_MIN, SHRT_MAX)
            blitpos_y = VIEW_Y+clamp_double(spr_y-offset_Y, SHRT_MIN, SHRT_MAX)

            
            if rect_intersection_fast(srect_x0, srect_y0, srect_x1, srect_y1, blitpos_x, blitpos_y, blitpos_x+w, blitpos_y+h):
                blit_list.append((spr.image, (blitpos_x, blitpos_y)))



    else:                                                               # no need to return dirty rects if not needed

        for i in range(spr_list_len):

            spr = spritelist[i]
            spr_rect = spr.rect
            spr_x = spr_rect.r.x
            spr_y = spr_rect.r.y
            w = spr_rect.r.w
            h = spr_rect.r.h

            blitpos_x = VIEW_X+clamp_double(spr_x-X, SHRT_MIN, SHRT_MAX)
            blitpos_y = VIEW_Y+clamp_double(spr_y-Y, SHRT_MIN, SHRT_MAX)

            if rect_intersection_fast(srect_x0, srect_y0, srect_x1, srect_y1, blitpos_x, blitpos_y, blitpos_x+w, blitpos_y+h):
                blit_list.append((spr.image, (blitpos_x, blitpos_y)))
    
   
    #t0 = perf_counter()
    
    dirt_rects = surface.blits(blit_list, doreturn=return_dirty_rects)
    
    #dt = perf_counter()-t0
    #print(dt)
    if return_dirty_rects:
        for i in range(len(dirt_rects)):

            rec = spritedict[spritelist[i]]

            newrect = dirt_rects[i]

            if rec is init_rect:
                dirty_append(newrect)
            else:
                if newrect.colliderect(rec):
                    dirty_append(newrect.union(rec))
                else:
                    dirty_append(newrect)
                    dirty_append(rec)
            
            spritedict[spritelist[i]] = newrect
    
    set_clip(None)                                                    # remove clipping
    #dt = perf_counter()-t0
    #print(dt)
    return dirty if return_dirty_rects else None


@cython.cdivision(True)
cpdef cython_fastdraw_2(object cameraview, Camera2 camera_object, Renderer renderer, Rect custom_area=None, int parallax=1, int use_float_coords=0):

    """
    Custom function for drawing many sprites at a time, using SDL2's hardware-accelerated rendering.
    
    :param object cameraview: the spritegroup used by a Camera2 object.
    :param Camera2 camera_object: a Camera2 object
    :param Renderer renderer: a pygame Renderer object
    :param int parallax: use per_object parallax

    :int use_float_coords: an alternate mode which can use floating-point coordinates to draw sprites, using their '_x' and '_y' attributes

    """
        
    cdef SDL_Rect *cspr_rect = NULL
    cdef SDL_Rect *csrcrect = NULL
    cdef SDL_Rect cdstrect
    cdef SDL_Point origin

    cdef SDL_FRect cdstfrect
    cdef SDL_FPoint forigin

    cdef Rect screenrect, spr_rect, init_rect, newrect, dest_rect
    cdef object spr
    cdef Sprite cspr 
    cdef object old_target = renderer.target

    cdef Py_ssize_t i
    
    cdef double parallax_ratio, spr_distance, srect_x0, srect_y0, srect_x1, srect_y1, spr_x, spr_y, blitpos_x, blitpos_y, t0, dt
    cdef int flip, res, w, h

    cdef bint flip_X, flip_Y, using_cspr

    cdef Image img
    cdef Texture tex

    cdef double offset_X = 0.0 
    cdef double offset_Y = 0.0
    cdef list spritelist = <list>cameraview._spritelist  

    cdef bint using_target_tex = False
                    
    screenrect = camera_object._screenrect
    srect_x0 = 0.0
    srect_y0 = 0.0
    srect_x1 = (screenrect.r.w)
    srect_y1 = (screenrect.r.h)

    cdef list dirty = cameraview.lostsprites
    cameraview.lostsprites = []

    init_rect = cameraview._init_rect

    cdef double X = camera_object._x  # floating-point Camera2 world-coordinates
    cdef double Y = camera_object._y

    cdef double view_distance = camera_object._view_distance       # Camera2 maximum view distance, needed for parallax displacement
    cdef double last_distace = 0.0

    cdef int spr_list_len = len(spritelist)

    if isinstance(camera_object._target_tex, Texture):         # Target-texture mode, allows for drawing multiple cameras in the same viewport 
        renderer.target = <Texture>camera_object._target_tex
        renderer.clear()
        using_target_tex = True

    if parallax:
        if use_float_coords:
            for i in range(spr_list_len):

                spr = <object>PyList_GET_ITEM(spritelist, i)

                if spr is None:                            # None-check optimisation when using the locked-mode of a CameraView object
                    continue

                elif isinstance(spr, Sprite):                                           
                    using_cspr = True
                    cspr = <Sprite>spr
                    spr_rect = cspr.rect
                    spr_x = cspr._x
                    spr_y = cspr._y
                    w = spr_rect.r.w
                    h = spr_rect.r.h

                    spr_distance = cspr.distance
                    #print "csprite"
                
                else:                                               # The sprite type is not from this file, less optimisations possible
                    using_cspr = False
                    spr_rect = <Rect>spr.rect
                    spr_x = spr.x
                    spr_y = spr.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h

                    spr_distance = spr.distance


                if spr_distance != last_distace:                                     
                    parallax_ratio = 1.0-clamp_double(fabs(spr_distance/view_distance))     # percentage to multiply sprite displacement by, the higher the sprite distance, the lower the displacement.

                    offset_X = parallax_ratio*X
                    offset_Y = parallax_ratio*Y

                    last_distace = spr_distance

                blitpos_x = spr_x-offset_X
                blitpos_y = spr_y-offset_Y

                if not ( (srect_x0 < blitpos_x+w) * (srect_x1 > blitpos_x) * (srect_y0 < blitpos_y+h) * (srect_y1 > blitpos_y) ):   # view culling to prevent excessive draw commands
                    continue
                elif using_cspr:
                    img = <Image>cspr._tex_img
                else:
                    img = <Image>spr.image
                
                tex = img.texture

                res = SDL_SetTextureColorMod(tex._tex,
                                    img.color.data[0],
                                    img.color.data[1],
                                    img.color.data[2])
                if res < 0:
                    raise error()

                res = SDL_SetTextureAlphaMod(tex._tex, <Uint8>img.alpha)
                
                if res < 0:
                    raise error()

                csrcrect = &img.srcrect.r
                cdstfrect.x = clamp_double_to_float(blitpos_x, -FLT_MAX, FLT_MAX)
                cdstfrect.y = clamp_double_to_float(blitpos_y, -FLT_MAX, FLT_MAX)
                cdstfrect.w = <float>w
                cdstfrect.h = <float>h
                
                flip_X = img.flipX
                flip_Y = img.flipY
                
                if ( (img.angle % 360 != 0) | (flip_X) | (flip_Y) ):

                    forigin.x = img.origin[0]
                    forigin.y = img.origin[1]
                                        
                    flip = (SDL_FLIP_NONE | SDL_FLIP_HORIZONTAL*flip_X | SDL_FLIP_VERTICAL*flip_Y)
                
                    res = SDL_RenderCopyExF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect, img.angle, &forigin, <SDL_RendererFlip>flip)
                else:
                    res = SDL_RenderCopyF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect)
                
                if res < 0:
                    raise error()
        
        else:
            for i in range(spr_list_len):
                            
                spr = <object>PyList_GET_ITEM(spritelist, i)

                if spr is None:                            # None-check optimisation when using the locked-mode of a CameraView object         
                    continue  

                elif isinstance(spr, Sprite):
                    using_cspr = True
                    cspr = <Sprite>spr
                    spr_rect = cspr.rect
                    spr_x = spr_rect.r.x
                    spr_y = spr_rect.r.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h

                    spr_distance = cspr.distance
                    #print "csprite"
                
                else:                                               # The sprite type is not from this file, less optimisations possible
                    using_cspr = False
                    spr_rect = <Rect>spr.rect
                    spr_x = spr_rect.r.x
                    spr_y = spr_rect.r.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h

                    spr_distance = spr.distance
                
    
                if spr_distance != last_distace:                                     
                    parallax_ratio = 1.0-clamp_double(fabs(spr_distance/view_distance))     # percentage to multiply sprite displacement by, the higher the sprite distance, the lower the displacement.
                
                    offset_X = parallax_ratio*X
                    offset_Y = parallax_ratio*Y
    
                    last_distace = spr_distance
    
                blitpos_x = spr_x-offset_X
                blitpos_y = spr_y-offset_Y
                
                if not ( (srect_x0 < blitpos_x+w) * (srect_x1 > blitpos_x) * (srect_y0 < blitpos_y+h) * (srect_y1 > blitpos_y) ): # view culling to prevent excessive draw commands
                    continue
                elif using_cspr:
                    img = <Image>cspr._tex_img
                else:
                    img = <Image>spr.image
                
                tex = img.texture

                res = SDL_SetTextureColorMod(tex._tex,
                                    img.color.data[0],
                                    img.color.data[1],
                                    img.color.data[2])
                if res < 0:
                    raise error()

                res = SDL_SetTextureAlphaMod(tex._tex, <Uint8>img.alpha)
                
                if res < 0:
                    raise error()

                csrcrect = &img.srcrect.r
                
                cdstfrect.x = clamp_double_to_float(blitpos_x, -FLT_MAX, FLT_MAX)
                cdstfrect.y = clamp_double_to_float(blitpos_y, -FLT_MAX, FLT_MAX)
                cdstfrect.w = <float>w
                cdstfrect.h = <float>h

                flip_X = img.flipX
                flip_Y = img.flipY
                
                if ( (img.angle % 360 != 0) | (flip_X) | (flip_Y) ):

                    forigin.x = img.origin[0]
                    forigin.y = img.origin[1]
                                        
                    flip = (SDL_FLIP_NONE | SDL_FLIP_HORIZONTAL*flip_X | SDL_FLIP_VERTICAL*flip_Y)
                
                    res = SDL_RenderCopyExF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect, img.angle, &forigin, <SDL_RendererFlip>flip)
                else:
                    res = SDL_RenderCopyF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect)
                
                if res < 0:
                    raise error()
    
    else:
        if use_float_coords:
            for i in range(spr_list_len):

                spr = spr = <object>PyList_GET_ITEM(spritelist, i)

                if spr is None:                                  # None-check optimisation when using the locked-mode of a CameraView object
                    continue

                elif isinstance(spr, Sprite):
                    using_cspr = True
                    cspr = <Sprite>spr
                    spr_rect = cspr.rect
                    spr_x = cspr._x
                    spr_y = cspr._y
                    w = spr_rect.r.w
                    h = spr_rect.r.h
                    #print "csprite"
                
                else:                                               # The sprite type is not from this file, less optimisations possible
                    using_cspr = False
                    spr_rect = <Rect>spr.rect
                    spr_x = spr.x
                    spr_y = spr.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h

                blitpos_x = spr_x-X
                blitpos_y = spr_y-Y

                if not ( (srect_x0 < blitpos_x+w) * (srect_x1 > blitpos_x) * (srect_y0 < blitpos_y+h) * (srect_y1 > blitpos_y) ): # view culling to prevent excessive draw commands
                    continue
                elif using_cspr:
                    img = <Image>cspr._tex_img
                else:
                    img = <Image>spr.image
                
                tex = img.texture

                res = SDL_SetTextureColorMod(tex._tex,
                                    img.color.data[0],
                                    img.color.data[1],
                                    img.color.data[2])
                if res < 0:
                    raise error()

                res = SDL_SetTextureAlphaMod(tex._tex, <Uint8>img.alpha)
                
                if res < 0:
                    raise error()

                csrcrect = &img.srcrect.r
                
                cdstfrect.x = clamp_double_to_float(blitpos_x, -FLT_MAX, FLT_MAX)
                cdstfrect.y = clamp_double_to_float(blitpos_y, -FLT_MAX, FLT_MAX)
                cdstfrect.w = <float>w
                cdstfrect.h = <float>h

                flip_X = img.flipX
                flip_Y = img.flipY
                
                if ( (img.angle % 360 != 0) | (flip_X) | (flip_Y) ):

                    forigin.x = img.origin[0]
                    forigin.y = img.origin[1]
                                        
                    flip = (SDL_FLIP_NONE | SDL_FLIP_HORIZONTAL*flip_X | SDL_FLIP_VERTICAL*flip_Y)
                
                    res = SDL_RenderCopyExF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect, img.angle, &forigin, <SDL_RendererFlip>flip)
                else:
                    res = SDL_RenderCopyF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect)
                
                if res < 0:
                    raise error()
        
        else:
            for i in range(spr_list_len):
    
                spr = spr = <object>PyList_GET_ITEM(spritelist, i)

                if spr is None:                                  # None-check optimisation when using the locked-mode of a CameraView object
                    continue

                elif isinstance(spr, Sprite):
                    using_cspr = True
                    cspr = <Sprite>spr
                    spr_rect = cspr.rect
                    spr_x = spr_rect.r.x
                    spr_y = spr_rect.r.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h
                    #print "csprite"
                else:                                               # The sprite type is not from this file, less optimisations possible
                    using_cspr = False
                    spr_rect = <Rect>spr.rect
                    spr_x = spr_rect.r.x
                    spr_y = spr_rect.r.y
                    w = spr_rect.r.w
                    h = spr_rect.r.h
        
                blitpos_x = spr_x-X
                blitpos_y = spr_y-Y
    
                if not ( (srect_x0 < blitpos_x+w) * (srect_x1 > blitpos_x) * (srect_y0 < blitpos_y+h) * (srect_y1 > blitpos_y) ): # view culling to prevent excessive draw commands
                    continue
                elif using_cspr:
                    img = <Image>cspr._tex_img
                else:
                    img = <Image>spr.image
                
                tex = img.texture

                res = SDL_SetTextureColorMod(tex._tex,
                                    img.color.data[0],
                                    img.color.data[1],
                                    img.color.data[2])
                if res < 0:
                    raise error()

                res = SDL_SetTextureAlphaMod(tex._tex, <Uint8>img.alpha)
                
                if res < 0:
                    raise error()

                csrcrect = &img.srcrect.r
                
                cdstfrect.x = clamp_double_to_float(blitpos_x, -FLT_MAX, FLT_MAX)
                cdstfrect.y = clamp_double_to_float(blitpos_y, -FLT_MAX, FLT_MAX)
                cdstfrect.w = <float>w
                cdstfrect.h = <float>h

                flip_X = img.flipX
                flip_Y = img.flipY
                
                if ( (img.angle % 360 != 0) | (flip_X) | (flip_Y) ):

                    forigin.x = img.origin[0]
                    forigin.y = img.origin[1]
                                        
                    flip = (SDL_FLIP_NONE | SDL_FLIP_HORIZONTAL*flip_X | SDL_FLIP_VERTICAL*flip_Y)
                
                    res = SDL_RenderCopyExF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect, img.angle, &forigin, <SDL_RendererFlip>flip)
                else:
                    res = SDL_RenderCopyF(tex.renderer._renderer, tex._tex, csrcrect, &cdstfrect)
                
                if res < 0:
                    raise error()

    if using_target_tex:
        renderer.target = old_target
        camera_object._target_tex.draw(dstrect=screenrect if custom_area is None else custom_area)


cdef class CameraView(LayeredUpdates):

    """subclass of LayeredUpdates specialized for use in Camera2 objects."""

    cdef bint locked
    cdef dict locked_sprite_indices
    cdef int spritecount

    def __cinit__(self):
        self.locked_sprite_indices = {}

    

    cpdef void lock(self):
        """
        Lock this CameraView object, in order to enable fixed size optimisations for sprite darwing routines.
        """
        self.locked = True

        cdef Py_ssize_t i
        cdef object spr

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist
        cdef int l = len(spritelist)
        self.spritecount = l
        
        for i in range(l):
            spr = spritelist[i]
            locked_sprite_indices[spr] = i

    
    cpdef bint is_locked(self):
        return self.locked
    
    
    cpdef void unlock(self) except *:

        if not self.locked:
            return
        
        self.locked = False

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist
        cdef int l = len(spritelist)

        if self.spritecount != l:
            raise pygame.error("CameraView sprite containers were modified while it was locked")

        for spr in locked_sprite_indices:
            spritelist[<Py_ssize_t>locked_sprite_indices[spr]] = spr

        locked_sprite_indices.clear()


    cpdef hide_single(self, sprite, bint ignore_unregistered=False):

        """
        Set a sprite to be None in this CameraView object when drawing, so that it is invisible, for better performance. This requires it to be locked.
        """
        
        if not self.locked:
            raise pygame.error("hiding and unhiding sprites is only possible when this CameraView object is locked")

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist
        cdef Py_ssize_t idx

        if (sprite not in locked_sprite_indices) and (not ignore_unregistered):
            raise ValueError("the given sprite was not added to this CameraView group.")
        
        idx = <Py_ssize_t>locked_sprite_indices[sprite]
        spritelist[idx] = None


    cpdef int check_visibility(self, Camera2 camera, double x, double y, int w, int h, double distance=0.0, object sprite=None, bint hide_invisible=False) except *:

        """
        Check if a set of rectangle coordinates would be visible on the screen.

        :param Camera2 camera: The camera object to test for.
        :param double x: x-coordinate of a rectangle.
        :param double y: y-coordinate of a rectangle.
        :param double w: width of a rectangle.
        :param double h: height of a rectangle.
        :param double distance: The parallax distance of the rectangle, for parallax effects. Defaults to 0.
        :param object sprite: an optional sprite to be hidden.
        :param bint hide_invisible: If True and the sprite parameter is not None, hide the given sprite object. 
        """
        
        cdef double parallax_ratio = 1.0-clamp_double(fabs(distance/camera._view_distance))

        cdef Rect cam_rect = camera._screenrect

        cdef double x2 = x-camera._x*parallax_ratio
        cdef double y2 = y-camera._y*parallax_ratio

        cdef bint visible = ( (0 < x2+w) * (cam_rect.r.w > x2) * (0 < y2+h) * (cam_rect.r.h > y2) )

        if (not visible) and (hide_invisible) and (sprite is not None):
            self.hide_single(sprite)

        return visible


    cpdef hide(self, list_or_tuple sprites, bint ignore_unregistered=False):

        """
        Set sprites to be None in this CameraView object when drawing, so that they are invisible, for better performance. This requires it to be locked.
        """

        if not self.locked:
            raise pygame.error("hiding and unhiding sprites is only possible when this CameraView object is locked")

        cdef list spr_list
        cdef tuple spr_tup

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist

        cdef object spr
        cdef int l
        cdef Py_ssize_t i
        cdef Py_ssize_t idx

        if isinstance(sprites, list):
            spr_list = <list>sprites
            l = len(spr_list)

            for i in range(l):
                spr = spr_list[i]
                if (spr not in locked_sprite_indices) and (not ignore_unregistered):
                    raise ValueError("one of the given sprites was not added to this CameraView group.")

                idx = <Py_ssize_t>locked_sprite_indices[spr]

                spritelist[idx] = None

        else:
            spr_tup = <tuple>sprites
            l = len(spr_tup)

            for i in range(l):
                spr = spr_tup[i]
                if (spr not in locked_sprite_indices) and (not ignore_unregistered):
                    raise ValueError("one of the given sprites was not added to this CameraView group.")

                idx = <Py_ssize_t>locked_sprite_indices[spr]

                spritelist[idx] = None


    cpdef unhide_single(self, sprite, bint ignore_unregistered=False):

        """
        Unhide previously hidden sprite objects.
        :param object sprite: The sprite to unhide.
        :param bint ignore_unregistered: if set to True, raise a ValueError if the given sprite was not in this CameraView object.  
        """
        
        if not self.locked:
            raise pygame.error("hiding and unhiding sprites is only possible when this CameraView object is locked")

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist
        cdef Py_ssize_t idx

        if (sprite not in locked_sprite_indices) and (not ignore_unregistered):
            raise ValueError("the given sprite was not added to this CameraView group.")

        idx = <Py_ssize_t>locked_sprite_indices[sprite]
        spritelist[idx] = None


    cpdef unhide(self, list_or_tuple sprites, bint ignore_unregistered=False):

        """
        Unhide previously hidden sprite objects.
        :param list_or_tuple sprites: The sprites to unhide.
        :param bint ignore_unregistered: if set to True, raise a ValueError if the given sprites were not in this CameraView object.  
        """

        if not self.locked:
            raise pygame.error("hiding and unhiding sprites is only possible when this CameraView object is locked")

        cdef list spr_list
        cdef tuple spr_tup

        cdef dict locked_sprite_indices = <dict>self.locked_sprite_indices
        cdef list spritelist = <list>self._spritelist

        cdef object spr
        cdef int l
        cdef Py_ssize_t i
        cdef Py_ssize_t idx

        if isinstance(sprites, list):
            spr_list = <list>sprites
            l = len(sprites)

            for i in range(l):
                spr = spr_list[i]
                if (spr not in locked_sprite_indices) and (not ignore_unregistered):
                    raise ValueError("one of the given sprites was not added to this CameraView group.")

                idx = <Py_ssize_t>locked_sprite_indices[spr]

                spritelist[idx] = spr

        else:
            spr_tup = <tuple>sprites
            l = len(sprites)

            for i in range(l):
                spr = spr_tup[i]
                if (spr not in locked_sprite_indices) and (not ignore_unregistered):
                    raise ValueError("one of the given sprites was not added to this CameraView group.")

                idx = <Py_ssize_t>locked_sprite_indices[spr]

                spritelist[idx] = spr
        

    cpdef void add_internal(self, sprite, layer=None):

        """Do not use this method directly.

        It is used by the group to add a sprite internally.
        """
        if self.locked:
            raise pygame.error("cannot add to this CameraView object when it is locked")


        self.spritedict[sprite] = self._init_rect

        if layer is None:
            try:
                layer = sprite._layer
            except AttributeError:
                layer = sprite._layer = self._default_layer
        
        elif hasattr(sprite, '_layer'):
            sprite._layer = layer

        if not hasattr(sprite, 'distance'):
            raise AttributeError("each sprite passed to a CameraView group must have a (.distance) attribute with a positive real number as a value.")
      
        cdef list sprites = self._spritelist # speedup
        cdef dict sprites_layers = self._spritelayers
        sprites_layers[sprite] = layer

        # add the sprite at the right position
        # bisect algorithmus
        cdef int low, mid, high 
        cdef int leng = len(sprites)
        low = mid = 0
        high = leng - 1
        cdef int layer0 = layer
        cdef int layer1

        while low <= high:
            mid = low + (high - low) // 2
            if sprites_layers[sprites[mid]] <= layer:
                low = mid + 1
            else:
                high = mid - 1
        # linear search to find final position
        while mid < leng and sprites_layers[sprites[mid]] <= layer:
            mid += 1
        sprites.insert(mid, sprite)



    def add(self, *sprites, **kwargs):
        
        """add a sprite or sequence of sprites to a group

        LayeredUpdates.add(*sprites, **kwargs): return None

        Each sprite must have a (.distance) attribute set to a real number above or equal to zero.

        If the sprite you add has an attribute _layer, then that layer will be
        used. If **kwarg contains 'layer', then the passed sprites will be
        added to that layer (overriding the sprite._layer attribute). If
        neither the sprite nor **kwarg has a 'layer', then the default layer is
        used to add the sprites.

        """

        if self.locked:
            raise pygame.error("cannot add to this CameraView object when it is locked")

        elif not sprites:
            return
        if 'layer' in kwargs:
            layer = kwargs['layer']
        else:
            layer = None

        cdef Sprite csprite
        for sprite in sprites:
            # It's possible that some sprite is also an iterator.
            # If this is the case, we should add the sprite itself,
            # and not the iterator object.
            if isinstance(sprite, Sprite):
                csprite = <Sprite>sprite
                if not self.has_internal(csprite):
                    self.add_internal(csprite, layer)
                    csprite.add_internal(self)
            else:
                try:
                    # See if sprite is an iterator, like a list or sprite
                    # group.
                    self.add(*sprite, **kwargs)
                except (TypeError, AttributeError):
                    # Not iterable. This is probably a sprite that is not an
                    # instance of the Sprite class or is not an instance of a
                    # subclass of the Sprite class. Alternately, it could be an
                    # old-style sprite group.
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if not self.has_internal(spr):
                                self.add_internal(spr, layer)
                                spr.add_internal(self)
                    elif not self.has_internal(sprite):
                        self.add_internal(sprite, layer)
                        sprite.add_internal(self)

    #cpdef draw(self, Camera2 camera_object, object surface, Rect custom_area=INIT_RECT, int parallax=0, int return_dirty_rects=1, int use_float_coords=0):
    #    """Do not use this method in production.
    #    It is used by Camera2 objects to draw sprites.
    #    """
    #    if isinstance(surface, Renderer):
    #        return cython_fastdraw_2(self, camera_object, <Renderer>surface, custom_area=custom_area, parallax=parallax, use_float_coords=use_float_coords)
    #    else:
    #        return cython_fastdraw(self, camera_object, surface, parallax=parallax, return_dirty_rects=return_dirty_rects)

    cpdef void remove_internal(self, sprite):
        """Do not use this method directly.

        The group uses it to add a sprite.

        """
        self._spritelist.remove(sprite)
        del self.spritedict[sprite]
        del self._spritelayers[sprite]
    

    def remove(self, *sprites):
        """remove sprite(s) from group

        Group.remove(sprite, list, or group, ...): return None

        Removes a sprite or sequence of sprites from a group.

        """
        # This function behaves essentially the same as Group.add. It first
        # tries to handle each argument as an instance of the Sprite class. If
        # that failes, then it tries to handle the argument as an iterable
        # object. If that failes, then it tries to handle the argument as an
        # old-style sprite group. Lastly, if that fails, it assumes that the
        # normal Sprite methods should be used.

        if self.locked:
            raise pygame.error("Cannot remove from this CameraView object when it is locked")


        cdef Sprite csprite
        for sprite in sprites:
            if isinstance(sprite, Sprite):
                csprite = <Sprite>sprite
                if self.has_internal(csprite):
                    self._spritelist.remove(csprite)
                    del self.spritedict[csprite]
                    del self._spritelayers[csprite]
                    csprite.remove_internal(self)
            else:
                try:
                    self.remove(*sprite)
                except (TypeError, AttributeError):
                    if hasattr(sprite, '_spritegroup'):
                        for spr in sprite.sprites():
                            if self.has_internal(spr):
                                self.remove_internal(spr)
                                spr.remove_internal(self)
                    elif self.has_internal(sprite):
                        self.remove_internal(sprite)
                        sprite.remove_internal(self)



    def change_layer(self, sprite, int new_layer):
        if self.locked:
            raise pygame.error("cannot modify the layers of this CameraView object when it is locked")

        LayeredUpdates.change_layer(self, sprite, new_layer)




#cdef class CameraViewData:





cdef class Camera2:

    cdef public int RETURN_POSITION_AS_FLOATS
    cdef dict __dict__
    cdef double _x, _y, _view_distance
    cdef public double dx, dy
    cdef public Rect _screenrect
    cdef public object view
    cdef list _area
    cdef object _target_tex, _anchor_point

    def __cinit__(self):
        self.__dict__ = {}

    def __init__(self, area, view=None, double view_distance=1000):

        """2D sprite camera class, with parallax support and rect-like behavior."""

        self.RETURN_POSITION_AS_FLOATS = 0  # for returning float-coordinates when preferred

        self._anchor_point = (0,0)

        self._view_distance = fmax(fabs(view_distance), 1.0)

        self.view = CameraView() if view is None else view
        if not isinstance(self.view, CameraView):
            t = TypeError("view parameter must be a CameraView object or None")
            raise t


        self._area = list(area) # Camera2 view area in 2D world space
        self._x = self._y = 0.0 # floating point coordinates, for more precise movement

        self._screenrect = Rect(area) # area on the screen that the camera draws to

        self.dx = 0
        self.dy = 0 

        self._target_tex = None


    cpdef void make_target_texture(self, Renderer renderer) except *:
        self._target_tex = Texture(renderer, (self._area[2], self._area[3]), target=1)

    cpdef object get_target_texture(self):
        return self._target_tex

    cpdef object draw(self, object surface, Rect custom_area=None, int parallax=1, int return_dirty_rects=1, int use_float_coords=0):
        if isinstance(surface, Renderer):
            return cython_fastdraw_2(self.view, self, <Renderer>surface, custom_area=custom_area, parallax=parallax, use_float_coords=use_float_coords)
        else:
            return cython_fastdraw(self.view, self, surface, parallax=parallax, return_dirty_rects=return_dirty_rects)

    cdef void _updatepos(self):
        self._x += self.dx
        self._y += self.dy

        self._area[0] = int(self._x)
        self._area[1] = int(self._y)
        
    cpdef object _g_srect(self):
        """ Camera 2 area on the screen (as a rect copy) """
        return self._screenrect.copy()
    
    cpdef object _s_srect(self, rect):
        cdef list area  = self._area
        self._screenrect = r = Rect(rect)
        self._area[2:] = r.size


    screenrect = property(_g_srect, _s_srect)



    



    cpdef void slide_to(self, object destination, double dt, double speed_factor=0.5, anchor_point=None):

        """slide a Camera2 object to a certain destination smoothly."""

        cdef double dest_x, dest_y, anch_x, anch_y
        cdef double fac = clamp_double(speed_factor*dt)
        
        if anchor_point is None:
            anchor_point = (0,0)
        else:
            anchor_point = tuple(anchor_point)

        dest_x = destination[0]
        dest_y = destination[1]
        anch_x = anchor_point[0]
        anch_y = anchor_point[1]
        
        self.dx = (dest_x-self._x-anch_x)*fac         # Modified Lerp formula 
        self.dy = (dest_y-self._y-anch_y)*fac

        self._updatepos()



    cpdef tuple _g_anchor_p(self):
        return self._anchor_point
    
    cpdef object _s_anchor_p(self, v):
        cdef int w, h
        w = self._screenrect.w
        h = self._screenrect.h
        try:
            self._anchor_point = (clamp_double(v[0]+0, 0, w), clamp_double(v[1]+0, 0, h))
            return
        except (TypeError, ValueError, IndexError):
            pass

        p = pygame.error("invalid assignment for Camera2 anchor point")
        raise p




    cpdef int _g_w(self):
        return self._area[2]
    
    cpdef object _s_w(self, v):
        try:
            self._area[2] = self._screenrect.w = int(v)
            return
        except ValueError:
            pass
        
        p = pygame.error("invalid assignment for Camera2 width")
        raise p


    cpdef int _g_h(self):
        return self._area[3]
    
    cpdef object _s_h(self, int v):
        try:
            self._area[3] = self._screenrect.h = int(v)
            
            return
        except ValueError:
            pass
        
        p = pygame.error("invalid assignment for Camera2 height")
        raise p

    cpdef tuple _g_sz(self):
        cdef list area  = self._area
        return (area[2], area[3])
    
    cpdef object _s_sz(self, v):
        try:
            self._area[2:] = self._screenrect.size = ( int(v[0]), int(v[1]) )
            
            return
        except (ValueError, TypeError):
            pass
        
        p = pygame.error("invalid assignment for Camera2 size")
        raise p
        
    
    cpdef _g_x(self):
        return self._x if self.RETURN_POSITION_AS_FLOATS else self._area[0] 
    
    cpdef object _s_x(self, double v):
        try:
            self.dx = v-self._x
            self.dy = 0
            
            self._updatepos()
            
            return
        except (TypeError, ValueError):
            pass
        
        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    
    cpdef _g_y(self):
        return self._y if self.RETURN_POSITION_AS_FLOATS else self._area[1] 
    
    cpdef object _s_y(self, double v):
        try:
            self.dy = v-self._y
            self.dx = 0
            self._updatepos()
            
            return
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    cpdef tuple _g_xy(self):
        return (self._x, self._y) if self.RETURN_POSITION_AS_FLOATS else (self._area[0], self._area[1])

    
    cpdef tuple _g_yx(self):
        return (self._y, self._x) if self.RETURN_POSITION_AS_FLOATS else (self._area[1], self._area[0])

    cpdef object _s_yx(self, v):
        try:
            self.dx = v[1]-self._x
            self.dy = v[0]-self._y
            self._updatepos()
            
            return
        except (TypeError, ValueError, IndexError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinates")
        raise p


    
    cpdef object _s_xy(self, v):
        try:
            
            self.dx = v[0]-self._x
            self.dy = v[1]-self._y
            
            self._updatepos()
            
            return
        except (TypeError, ValueError, IndexError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinates")
        raise p


    cpdef _g_ct(self):
        cdef list area  = self._area
        return area[0]+area[2]//2, area[1]+area[3]//2

    
    cpdef object _s_ct(self, v):

        cdef list area  = self._area
        try:
            #ct = (area[0]+area[2]//2,  area[1]+area[3]//2)
            self.dx = v[0]-(area[0]+area[2]//2)
            self.dy = v[1]-(area[1]+area[3]//2)
            
            self._updatepos()
            
            return
        except (TypeError, ValueError, IndexError):
            pass

        p = pygame.error("invalid assignment for Camera2 center")
        raise p

    cpdef int _g_r(self):
        cdef list area  = self._area
        return area[0]+area[2]
    
    cpdef object _s_r(self, v):

        cdef list area  = self._area

        try:
            self.dx = v-(area[0]+area[2])
            self.dy = 0
            self._updatepos()
            
            return
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p


    cpdef int _g_b(self):
        cdef list area  = self._area
        return area[1]+area[3]
    
    cpdef object _s_b(self, v):

        cdef list area  = self._area

        try:
            self.dy = v-(area[1]+area[3])
            self.dx = 0
            self._updatepos()
            
            return
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    cpdef tuple _g_tr(self):
        cdef list area  = self._area
        return area[0]+area[2], area[1]


    cpdef object _s_tr(self, v):

        cdef list area  = self._area

        try:
            self.dx = v[0]-(area[0]+area[2])
            self.dy = v[1]-(area[1])
            
            self._updatepos()
            return
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p


    cpdef tuple _g_bl(self):
        cdef list area  = self._area
        return area[0], area[1]+area[3]


    cpdef object _s_bl(self, v):

        cdef list area  = self._area

        try:
            self.dx = v[0]-(area[0])
            self.dy = v[1]-(area[1]+area[3])
            
            self._updatepos()
            return

        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p


    cpdef tuple _g_br(self):
        cdef list area  = self._area
        return area[0]+area[2], area[1]+area[3]


    cpdef object _s_br(self, v):

        cdef list area  = self._area

        try:
            self.dx = v[0]-(area[0]+area[2])
            self.dy = v[1]-(area[1]+area[3])            
            self._updatepos()
            return
            
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    cpdef tuple _g_mt(self):
        cdef list area = self._area
        return area[0]+area[2]//2, area[1]


    cpdef object _s_mt(self, v):

        cdef list area  = self._area

        try:
            self.dx = v[0]-(area[0]+area[2]//2)
            self.dy = v[1]-(area[1])
            
            self._updatepos()
            return
            
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    cpdef tuple _g_ml(self):
        cdef list area = self._area
        return area[0], area[1]+area[3]//2

    
    cpdef object _s_ml(self, v):

        cdef list area  = self._area

        try:
            self.dx = v[0]-(area[0])
            self.dy = v[1]-(area[1]+area[3]//2)
            
            self._updatepos()
            return
            
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    cpdef tuple _g_mb(self):
        cdef list area = self._area
        return area[0]+area[2]//2, area[1]+area[3]

    cpdef object _s_mb(self, v):

        cdef list area = self._area

        try:
            self.dx = v[0]-(area[0]+area[2]//2)
            self.dy = v[1]-(area[1]+area[3])
            
            self._updatepos()
            return
            
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p


    cpdef tuple _g_mr(self):
        cdef list area = self._area
        return area[0]+area[2], area[1]+area[3]//2
    
    
    cpdef object _s_mr(self, v):

        cdef list area = self._area

        try:
            self.dx = v[0]-(area[0]+area[2])
            self.dy = v[1]-(area[1]+area[3]//2)
            
            self._updatepos()
            return
            
        except (TypeError, ValueError):
            pass

        p = pygame.error("invalid assignment for Camera2 coordinate")
        raise p

    anchor = property(_g_anchor_p, _s_anchor_p)         # special anchor point for (.goto()) method

    topleft = xy = pos = property(_g_xy, _s_xy)
    topright = property(_g_tr, _s_tr)
    
    midtop = property(_g_mt, _s_mt)
    midleft  = property(_g_ml, _s_ml)
    midbottom  =  property(_g_mb, _s_mb)
    midright = property(_g_mr, _s_mr)
    
    bottomleft = property(_g_bl, _s_bl)
    bottomright =  property(_g_br, _s_br)
    
    center = middle = property(_g_ct, _s_ct)

    yx = property(_g_yx, _s_yx)
    x = left = property(_g_x, _s_x)
    y = top = property(_g_x, _s_x)
    right = property(_g_r, _s_r)
    bottom = property(_g_b, _s_b)

    width = w = property(_g_w, _s_w)
    height = h = property(_g_h, _s_h)
    size = property(_g_sz, _s_sz)  
    

    cpdef void move(self, dx_or_dy=None, dy=None):  # (dx_or_dy=None, dy=None) this allows the method to support iterables by indexing, as well as two numbers as input.

        if dx_or_dy is not None:
            if dy is not None:
                try:
                    self.dx=dx_or_dy; self.dy=dy
                    self._updatepos()
                    return
                except TypeError:
                    pass
            else:
                try:
                    self.dx = dx_or_dy[0]; self.dy = dx_or_dy[1]
                    self._updatepos()
                    return
                except TypeError:
                    try:
                        self.dx = self.dy = dx_or_dy
                        self._updatepos()
                        return
                    except TypeError:
                        pass
                except IndexError:
                    pass
        else:
            return
        
        t = TypeError("invalid coordinates given as input: ("+repr(dx_or_dy)+", "+repr(dy)+"). Inputs must be two real numbers, or a tuple, list or vector object with 2 coordinates.")
        raise t
    
    
    cpdef void goto(self, x_or_xy=None, y=None):
        """Move this Camera2 object to a certain location, based on its anchor."""

        if x_or_xy is not None:
            if y is not None:
                try:
                    self.dx=(x_or_xy)-self._x-self._anchor_point[0]; self.dy=(y)-self._y-self._anchor_point[1]
                    self._updatepos()
                    return
                except TypeError:
                    pass
            else:
                try:
                    self.dx=(x_or_xy[0])-self._x-self._anchor_point[0]; self.dy=(x_or_xy[1])-self._y-self._anchor_point[1]
                    self._updatepos()
                    return
                except TypeError:
                    try:
                        self.dx=(x_or_xy)-self._x-self._anchor_point[0]; self.dy=(x_or_xy)-self._y-self._anchor_point[1]
                        self._updatepos()
                        return
                    except TypeError:
                        pass
              
        
                except IndexError:
                    pass
        else:
            return
        
        t = TypeError("invalid coordinates given as input: ("+repr(x_or_xy)+", "+repr(y)+"). Inputs must be two real numbers, or a tuple, list or vector object with 2 coordinates.")
        raise t


    cpdef void teleport(self, x_or_xy=None, y=None):

        old_dx, old_dy = self.dx, self.dy
        passed = False

        if x_or_xy is not None:
            if y is not None:
                try:
                    self.dx=int(x_or_xy)-self._x-self._anchor_point[0]; self.dy=int(y)-self._y-self._anchor_point[1]
                    self._updatepos()
                    passed = True
                except TypeError:
                    pass
            else:
                try:
                    self.dx=int(x_or_xy[0])-self._x-self._anchor_point[0]; self.dy=int(x_or_xy[1])-self._y-self._anchor_point[1]
                    self._updatepos()
                    passed = True
                except TypeError:
                    try:
                        self.dx=int(x_or_xy)-self._x-self._anchor_point[0]; self.dy=int(x_or_xy)-self._y-self._anchor_point[1]
                        self._updatepos()
                        passed = True
                    except TypeError:
                        pass
        
                except IndexError:
                    pass
        else:
            return
        
        self.dx, self.dy = old_dx, old_dy
        
        if not passed:
            t = TypeError("invalid coordinates given as input: ("+repr(x_or_xy)+", "+repr(y)+"). Inputs must be two real numbers, or a tuple, list or vector object with 2 coordinates.")
            raise t

    
    tp = teleport


    cpdef tuple get_pos(self, precise=False):
        """Get the world postion of this Camera2 object."""
        cdef list area = self._area
        return (self._x, self._y) if precise else (area[0], area[1])


    cpdef void reset_pos(self):
        """Reset the world postion of this Camera2 object."""
        cdef list area = self._area
        area[:] = (0.0, 0.0, area[3], area[4])
        self._x = self._y = 0.0


    cpdef tuple get_area(self):
        """Return the area of this Camera2 object in world-space."""
        cdef list area = self._area
        return (area[0], area[1], area[2], area[3])


    cpdef void set_size(self, x_or_xy=None, y=None):
        """Set the view size of this Camera2 object."""
        passed = False
        if x_or_xy is not None:
            if y is not None:
                try:
                    self._area[2:] = self._screenrect.size = x_or_xy+0, y+0
                    passed = True                    
                except TypeError:
                    pass
            else:
                try:
                    self._area[2:] = self._screenrect.size = x_or_xy[0]+0, x_or_xy[1]+0
                    passed = True
                except TypeError:
                    try:
                        self._area[2:] = self._screenrect.size = x_or_xy.x+0, x_or_xy.y+0
                        passed = True
                    except AttributeError:
                        try:
                            self._area[2:] = self._screenrect.size = (x_or_xy+0, x_or_xy+0)
                            passed = True
                        except Exception:
                            pass
                except IndexError:
                    pass
        else:
            return


        if not passed:
            t = TypeError("invalid coordinates given as input: ("+repr(x_or_xy)+", "+repr(y)+"). Inputs must be two real numbers, or a tuple, list or vector object with 2 coordinates.")
            raise t

    
    cpdef double _g_v_dist(self):
        return self._view_distance

    cpdef object _s_v_dist(self, double d):
        try:
            self._view_distance = fmax(fabs(d), 1)
        except TypeError as t:
            t.args = ("invalid input for 'view_distance' parameter, it must be a positive real number",)
            raise t from None

    view_distance = property(_g_v_dist, _s_v_dist)
    
    def __str__(self):
        return "<(Camera2 | area: {0}, at [{1:.5f}, {2:.5f}])>".format(tuple(self._screenrect), self._x, self._y)



cdef class CharMap:

    cdef readonly Texture charmap_tex
    cdef readonly dict charmap_rect_dict
    cdef readonly object font
    
    def __init__(self, Renderer renderer, object font_obj, object font_color=Color(255, 255, 255, 0), object charmap_size=(512, 512), bint aa=True, str characters=" 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'()*+,-./:;<=>?@[\]^_`´{|}~*°²³©�"):

        if PY_MAJOR_VERSION < 3:
            raise NotImplementedError("this class is not supported by Python versions below Python 3.0")

        if '�' not in characters:
            characters+='�'

        elif not characters:
            raise ValueError("no valid character string supplied")

        cdef str word = characters
        cdef list letter_surfaces = []
        cdef list letter_rects = []
        cdef list letters = []
        cdef dict charmap_rects = {}
        cdef tuple letter_surf_size

        cdef int x = 0
        cdef int y = 0

        charmap_surf = Surface(charmap_size, pygame.SRCALPHA)

        cdef int max_width = charmap_surf.get_width()
        cdef int max_height = charmap_surf.get_height()
        
        render = font_obj.render

        cdef int letter_width, letter_height
        
        for i in range(len(word)):
            try:
                letter_surfaces.append(render(word[i], aa, font_color))
            except pygame.error:
                continue
            else:
                letters.append(word[i])

        for letter_surf in letter_surfaces:
            letter_surf_size = <tuple>letter_surf.get_size()
            letter_width = <int>letter_surf_size[0]
            letter_height = <int>letter_surf_size[1]
            
            if x + letter_width >= max_width:
                x = 0  # Reset the x coordinate.
                y += letter_height  # Start on a new row.
            
            charmap_surf.blit(letter_surf, (x, y))
            letter_rects.append(Rect(x, y, letter_width, letter_height))

            x += letter_width

        letter_surfaces.clear()
        
        for i in range(len(letters)):
            charmap_rects[letters[i]] = letter_rects[i]

        self.charmap_tex = Texture.from_surface(renderer, charmap_surf)
        self.charmap_rect_dict = <dict>charmap_rects
        self.font = font_obj


    def render_string_to_texture(self, Renderer renderer not None, str text_string not None, Texture destination_target_tex not None, object color=Color(255, 255, 255, 0), int alpha=255, bint word_wrapping=False, int line_spacing=-1, int word_spacing_offset=0, int char_spacing_offset=0, bint use_font_size=False, float font_size=12.0, float char_scale_x=1.0, float char_scale_y=1.0, Rect text_box_rect=None):

        if word_wrapping:
            self._render_string_to_texture_3(renderer, text_string, destination_target_tex, color=color, alpha=alpha, line_spacing=line_spacing, text_box_rect=text_box_rect, word_spacing_offset=word_spacing_offset, char_spacing_offset=char_spacing_offset, use_font_size=use_font_size, font_size=font_size, char_scale_x=char_scale_x, char_scale_y=char_scale_y)
        else:
            self._render_string_to_texture_1(renderer, text_string, destination_target_tex, color=color, alpha=alpha, line_spacing=line_spacing, text_box_rect=text_box_rect, char_spacing_offset=char_spacing_offset, use_font_size=use_font_size, font_size=font_size, char_scale_x=char_scale_x, char_scale_y=char_scale_y)


    
    cpdef void _render_string_to_texture_3(self, Renderer renderer, str text_string, Texture destination_target_tex, object color=Color(255, 255, 255, 0), int alpha=255, int line_spacing=-1, int word_spacing_offset=0, int char_spacing_offset=0, bint use_font_size=False, float font_size=12.0, float char_scale_x=1.0, float char_scale_y=1.0, Rect text_box_rect=None) except*:

        cdef Py_ssize_t i, j, k, l

        cdef Py_ssize_t start_word_idx = 0

        i = j = k = l = 0
        cdef str word, character, word_char, somechar

        cdef str newline = "\n"
        cdef str whitespace = " "
        cdef str empty_str = ""
        cdef str UNKNOWN_CHAR = <str>chr(0xFFFD)

        cdef tuple word_tup

        cdef object old_color_mod = self.charmap_tex.color
        cdef Uint8 old_alpha_mod = self.charmap_tex.alpha

        self.charmap_tex.color = color
        self.charmap_tex.alpha = <Uint8>alpha

        cdef dict char_rect_dict = self.charmap_rect_dict
        cdef Texture char_atlas_tex = self.charmap_tex

        cdef Rect word_char_rect, char_rect

        cdef SDL_Rect view_rect
        cdef SDL_Rect cdstrect
        cdef SDL_Rect* cdstrect_ptr = &cdstrect
        cdef SDL_Rect* csrcrect_ptr = NULL

        cdef int res

        cdef Texture old_target = renderer.target

        cdef bint is_last_char = 0

        cdef tuple chars_tuple = tuple(text_string)
        cdef int chars_tuple_len = len(chars_tuple)

        cdef int character_len, word_char_list_len

        cdef int font_height = <int>self.font.get_height()

        cdef float char_scalex = char_scale_x
        cdef float char_scaley = char_scale_y

        cdef float font_pt_to_px = (font_size*1.333333)/font_height
        
        if use_font_size:
            char_scalex *= font_pt_to_px
            char_scaley *= font_pt_to_px
        
        cdef int word_width = 0
        cdef int word_range = 0 

        cdef int range_start, range_value

        range_start = range_value = 0

        cdef Rect space_rect = char_rect_dict[whitespace]
        cdef int space_rect_w = space_rect.r.w
        space_rect_w = <int>(space_rect_w * char_scalex)

        cdef Rect unknown_rect = char_rect_dict[UNKNOWN_CHAR]

        
        cdef int* word_range_arr = <int*>PyMem_Malloc(chars_tuple_len*sizeof(int))
        cdef int word_range_arr_idx = 0
        
        cdef SDL_Rect* char_sdlrect_arr = <SDL_Rect*>PyMem_Malloc(chars_tuple_len*sizeof(SDL_Rect))
        cdef int char_sdlrect_arr_idx = 0

        
        if (not word_range_arr) or (not char_sdlrect_arr):
            raise MemoryError()

        for i in range(chars_tuple_len):
            is_last_char = (i == chars_tuple_len-1)
            
            somechar = <str><object>PyTuple_GET_ITEM(chars_tuple, i)
            if (<bint>somechar.isspace()) or is_last_char:

                if is_last_char:
                    if somechar == newline:
                        
                        word_range = i-start_word_idx

                        word_range_arr[word_range_arr_idx] = word_range # append
                        word_range_arr_idx += 1

                        word_range_arr[word_range_arr_idx] = 1 # append
                        word_range_arr_idx += 1
                        break
                        
                    else:
                        word_range = (i+1)-start_word_idx
                    
                else:
                    word_range = i-start_word_idx
                
                start_word_idx = i+1
                
                if word_range:
                    word_range_arr[word_range_arr_idx] = word_range # append
                    word_range_arr_idx += 1
                
                if not is_last_char:     
                    word_range_arr[word_range_arr_idx] = 1 # append
                    word_range_arr_idx += 1


        if line_spacing == -1:
            line_spacing = <int>self.font.get_linesize()

        line_spacing = <int>(line_spacing * char_scaley)

        if text_box_rect is None:

            view_rect.x = 0
            view_rect.y = 0
            view_rect.w = destination_target_tex.width
            view_rect.h = destination_target_tex.height

        else:

            view_rect.x = text_box_rect.r.x
            view_rect.y = text_box_rect.r.y
            view_rect.w = text_box_rect.r.w
            view_rect.h = text_box_rect.r.h

        cdstrect.x = view_rect.x
        cdstrect.y = view_rect.y
        cdstrect.w = 0
        cdstrect.h = 0

        renderer.target = destination_target_tex

        if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
            renderer.target = old_target
            self.charmap_tex.color = old_color_mod
            self.charmap_tex.alpha = old_alpha_mod

            PyMem_Free(word_range_arr)
            PyMem_Free(char_sdlrect_arr)
            
            return
        

        while j < word_range_arr_idx:

            range_value = word_range_arr[j]

            if range_value > 1:
            
                for k in range(range_start, range_start+range_value):
                    
                    word_char = <str><object>PyTuple_GET_ITEM(chars_tuple, k)

                    if not PyDict_Contains(<dict>char_rect_dict, word_char):
                        word_char = UNKNOWN_CHAR
                        char_rect = unknown_rect
                    else:
                        char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, word_char)

                    word_width += <int>(char_rect.r.w * char_scalex)

                    char_sdlrect_arr[char_sdlrect_arr_idx] = char_rect.r
                    char_sdlrect_arr_idx += 1

                cdstrect.x += word_spacing_offset

                if cdstrect.x + cdstrect.w + word_width > view_rect.w:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x

                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:

                    renderer.target = old_target
                    self.charmap_tex.color = old_color_mod
                    self.charmap_tex.alpha = old_alpha_mod

                    PyMem_Free(word_range_arr)
                    PyMem_Free(char_sdlrect_arr)
                    
                    return

                
                word_width = 0
                char_sdlrect_arr_idx = 0

                for l in range(range_value):

                    csrcrect_ptr = &char_sdlrect_arr[l]
                    
                    cdstrect.w = <int>(csrcrect_ptr.w * char_scalex)
                    cdstrect.h = <int>(csrcrect_ptr.h * char_scaley)

                    res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
                    if res < 0:
                        raise error()

                    cdstrect.x += cdstrect.w + char_spacing_offset

                    if cdstrect.x+cdstrect.w > view_rect.w:
                        cdstrect.y += line_spacing
                        cdstrect.x = view_rect.x
            
            else:

                character = <str><object>PyTuple_GET_ITEM(chars_tuple, range_start)

                if character == newline:

                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x
                    if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                        break
                    
                    range_start += range_value
                    j+=1
                    continue

                elif character == whitespace:

                    cdstrect.w = space_rect_w
                    
                    cdstrect.x += cdstrect.w + word_spacing_offset

                    if cdstrect.x+cdstrect.w > view_rect.w:
                        cdstrect.y += line_spacing
                        cdstrect.x = view_rect.x

                    if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                        break

                    range_start += range_value
                    j+=1
                    continue
                
                if not PyDict_Contains(<dict>char_rect_dict, character):
                    character = UNKNOWN_CHAR
                
                char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, character)
                csrcrect_ptr = &char_rect.r
                
                cdstrect.w = <int>(csrcrect_ptr.w * char_scalex)
                cdstrect.h = <int>(csrcrect_ptr.h * char_scaley)

                res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
                if res < 0:
                    raise error()

                cdstrect.x += cdstrect.w + word_spacing_offset
                if cdstrect.x+cdstrect.w > view_rect.w:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x

                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                    break

            
            range_start += range_value
            j+=1

        renderer.target = old_target

        self.charmap_tex.color = old_color_mod
        self.charmap_tex.alpha = old_alpha_mod

        PyMem_Free(char_sdlrect_arr)
        PyMem_Free(word_range_arr)



    
    cpdef void _render_string_to_texture_2(self, Renderer renderer, str text_string, Texture destination_target_tex, object color=Color(255, 255, 255, 0), int alpha=255, int line_spacing=-1, float word_spacing_mod=1.0, int char_spacing_offset=0, Rect text_box_rect=None) except*:

        cdef Py_ssize_t i, j, k, l

        cdef Py_ssize_t start_word_idx = 0

        i = j = k = l = 0
        cdef str word, character, word_char, somechar

        cdef str newline = "\n"
        cdef str whitespace = " "
        cdef str empty_str = ""
        cdef str UNKNOWN_CHAR = <str>chr(0xFFFD)

        cdef tuple word_tup

        cdef object old_color_mod = self.charmap_tex.color
        cdef Uint8 old_alpha_mod = self.charmap_tex.alpha

        self.charmap_tex.color = color
        self.charmap_tex.alpha = <Uint8>alpha

        cdef dict char_rect_dict = self.charmap_rect_dict
        cdef Texture char_atlas_tex = self.charmap_tex

        cdef Rect word_char_rect, char_rect

        cdef SDL_Rect view_rect
        cdef SDL_Rect cdstrect
        cdef SDL_Rect* cdstrect_ptr = &cdstrect
        cdef SDL_Rect* csrcrect_ptr = NULL

        cdef int res = 0

        cdef Texture old_target = renderer.target

        cdef list word_char_list = []
        cdef list word_char_rect_list = []

        cdef bint is_last_char = 0

        cdef tuple chars_tuple = tuple(text_string)
        cdef int chars_tuple_len = len(chars_tuple)

        cdef int character_len, word_char_list_len
        cdef int word_width = 0

        cdef Rect space_rect = char_rect_dict[whitespace]
        cdef Rect unknown_rect = char_rect_dict[UNKNOWN_CHAR]

        for i in range(chars_tuple_len):
            
            is_last_char = (i == chars_tuple_len-1)
            somechar = <str>chars_tuple[i]
            
            if somechar.isspace() or is_last_char:

                if is_last_char:
                    word = <str>empty_str.join(chars_tuple[start_word_idx:i+1])
                else:
                    word = <str>empty_str.join(chars_tuple[start_word_idx:i])
                    
                if somechar == newline:
                    word = newline if word == empty_str else word
                    word_char_list.append(word)

                    if word != newline and not is_last_char:
                        word_char_list.append(newline)

                else:
                    word = whitespace if word == empty_str else word
                    word_char_list.append(word)

                    if word != whitespace and not is_last_char:
                        word_char_list.append(whitespace)

                start_word_idx = i+1


        word_char_list_len = len(word_char_list)
        

        if line_spacing == -1:
            line_spacing = <int>self.font.get_linesize()

        if text_box_rect is None:

            view_rect.x = 0
            view_rect.y = 0
            view_rect.w = destination_target_tex.width
            view_rect.h = destination_target_tex.height

        else:

            view_rect.x = text_box_rect.r.x
            view_rect.y = text_box_rect.r.y
            view_rect.w = text_box_rect.r.w
            view_rect.h = text_box_rect.r.h

        cdstrect.x = view_rect.x
        cdstrect.y = view_rect.y
        cdstrect.w = 0
        cdstrect.h = 0

        renderer.target = destination_target_tex

        if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
            renderer.target = old_target
            self.charmap_tex.color = old_color_mod
            self.charmap_tex.alpha = old_alpha_mod
            return

        while j < word_char_list_len:
            
            character = <str><object>PyList_GET_ITEM(word_char_list, j)
            
            character_len = len(character)

            if not character_len:
                continue
            
            elif character_len > 1:

                word_tup = tuple(character)

                for k in range(character_len):
                    
                    word_char = <str><object>PyTuple_GET_ITEM(word_tup, k)

                    if not PyDict_Contains(<dict>char_rect_dict, word_char):
                        word_char = UNKNOWN_CHAR
                        char_rect = unknown_rect
                    else:
                        char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, word_char)

                    word_width += char_rect.r.w

                    word_char_rect_list.append(char_rect)


                if cdstrect.x + cdstrect.w + word_width > view_rect.w:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x

                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:

                    renderer.target = old_target
                    self.charmap_tex.color = old_color_mod
                    self.charmap_tex.alpha = old_alpha_mod
                    return

                word_width = 0

                for l in range(character_len):

                    word_char_rect = word_char_rect_list[l]

                    csrcrect_ptr = &word_char_rect.r
                    
                    cdstrect.w = csrcrect_ptr.w
                    cdstrect.h = csrcrect_ptr.h

                    res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
                    if res < 0:
                        raise error()

                    cdstrect.x += cdstrect.w

                    if cdstrect.x+cdstrect.w > view_rect.w:
                        cdstrect.y += line_spacing
                        cdstrect.x = view_rect.x

                word_char_rect_list.clear()

            else:

                if character == newline:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x
                    if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                        break
                    
                    j+=1
                    continue

                elif character == whitespace:
                    cdstrect.w = space_rect.r.w
                    
                    cdstrect.x += cdstrect.w

                    if cdstrect.x+cdstrect.w > view_rect.w:
                        cdstrect.y += line_spacing
                        cdstrect.x = view_rect.x

                    if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                        break

                    j+=1
                    continue
                
                if not PyDict_Contains(<dict>char_rect_dict, character):
                    character = UNKNOWN_CHAR
                
                char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, character)
                csrcrect_ptr = &char_rect.r
                
                cdstrect.w = csrcrect_ptr.w
                cdstrect.h = csrcrect_ptr.h

                res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
                
                if res < 0:
                    raise error()

                cdstrect.x += cdstrect.w
                if cdstrect.x+cdstrect.w > view_rect.w:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x

                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                    break

            j+=1

        renderer.target = old_target

        self.charmap_tex.color = old_color_mod
        self.charmap_tex.alpha = old_alpha_mod

        



    
    
    
    
    
    cpdef void _render_string_to_texture_1(self, Renderer renderer, str text_string, Texture destination_target_tex, object color=Color(255, 255, 255, 0), int alpha=255, int line_spacing=-1, int char_spacing_offset=0, bint use_font_size=False, float font_size=12.0, float char_scale_x=1.0, float char_scale_y=1.0, Rect text_box_rect=None) except*:

        #if PY_MAJOR_VERSION < 3:
        #    return
    
        #cdef double t0, dt
        #
        #t0 = <double>perf_counter()

        cdef Py_ssize_t i
        cdef str character

        cdef str newline = "\n"
        cdef str whitespace = " "
        cdef str empty_str = ""
        cdef str UNKNOWN_CHAR = <str>chr(0xFFFD)

        cdef object old_color_mod = self.charmap_tex.color
        cdef Uint8 old_alpha_mod = self.charmap_tex.alpha

        self.charmap_tex.color = color
        self.charmap_tex.alpha = <Uint8>alpha

        cdef dict char_rect_dict = self.charmap_rect_dict
        
        cdef Texture char_atlas_tex = self.charmap_tex

        cdef SDL_Rect view_rect
        cdef SDL_Rect cdstrect
        cdef SDL_Rect* cdstrect_ptr = &cdstrect
        cdef SDL_Rect* csrcrect_ptr = NULL

        cdef int res

        cdef int font_height = <int>self.font.get_height()

        cdef float char_scalex = char_scale_x
        cdef float char_scaley = char_scale_y

        cdef float font_pt_to_px = (font_size*1.333333)/font_height
        
        if use_font_size:
            char_scalex *= font_pt_to_px
            char_scaley *= font_pt_to_px
        

        cdef tuple chars_tuple = tuple(text_string)
        cdef int l = len(chars_tuple)

        cdef Texture old_target = renderer.target

        cdef Rect space_rect = char_rect_dict[whitespace]
        cdef int space_rect_w = space_rect.r.w
        space_rect_w = <int>(space_rect_w * char_scalex)

        cdef Rect unknown_rect = char_rect_dict[UNKNOWN_CHAR]

        cdef bint UNDERLINE = <int>self.font.get_underline()
        cdef bint ITALIC = <int>self.font.get_italic()

        if line_spacing == -1:
            line_spacing = <int>self.font.get_linesize()
        
        line_spacing = <int>(line_spacing * char_scaley)

        if text_box_rect is None:

            view_rect.x = 0
            view_rect.y = 0
            view_rect.w = destination_target_tex.width
            view_rect.h = destination_target_tex.height

        else:

            view_rect.x = text_box_rect.r.x
            view_rect.y = text_box_rect.r.y
            view_rect.w = text_box_rect.r.w
            view_rect.h = text_box_rect.r.h

        cdstrect.x = view_rect.x
        cdstrect.y = view_rect.y
        cdstrect.w = 0
        cdstrect.h = 0

        renderer.target = destination_target_tex

        if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
            renderer.target = old_target
            return

        for i in range(l):
            
            character = <str><object>PyTuple_GET_ITEM(chars_tuple, i)

            if character == newline:
                cdstrect.y += line_spacing
                cdstrect.x = view_rect.x
                
                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                    break
                
                continue

            elif character == whitespace and not UNDERLINE:
                
                cdstrect.w = space_rect_w
                cdstrect.x += cdstrect.w + char_spacing_offset

                if cdstrect.x+cdstrect.w > view_rect.w:
                    cdstrect.y += line_spacing
                    cdstrect.x = view_rect.x

                if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                    break

                continue

            
            if not PyDict_Contains(<dict>char_rect_dict, character):
                character = UNKNOWN_CHAR
                char_rect = unknown_rect
            else:
                char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, character)
            
            csrcrect_ptr = &char_rect.r
            
            cdstrect.w = <int>(csrcrect_ptr.w * char_scalex)
            cdstrect.h = <int>(csrcrect_ptr.h * char_scaley)

            res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
            if res < 0:
                raise error()

            cdstrect.x += cdstrect.w + char_spacing_offset
            if cdstrect.x+cdstrect.w > view_rect.w:
                cdstrect.y += line_spacing
                cdstrect.x = view_rect.x

            if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                break

        renderer.target = old_target

        self.charmap_tex.color = old_color_mod
        self.charmap_tex.alpha = old_alpha_mod

        #dt = <double>perf_counter()-t0
        #print dt















def render_string_to_texture(Renderer renderer, str text_string, Texture destination_target_tex not None, Texture char_atlas_tex not None, dict charmap_rect_dict not None, int line_spacing, Rect text_box_rect=None):
    
    if PY_MAJOR_VERSION < 3:
        return
    
    cdef Py_ssize_t i
    cdef str character

    cdef str UNKNOWN_CHAR = <str>chr(0xFFFD)

    cdef Rect char_rect
    cdef SDL_Rect view_rect

    char_rect_dict = <dict>charmap_rect_dict
    if not char_rect_dict:
        raise ValueError("string to Rect dictionary is empty")

    cdef SDL_Rect cdstrect
    cdef SDL_Rect* cdstrect_ptr = &cdstrect
    cdef SDL_Rect* csrcrect_ptr = NULL

    cdef int res = 0

    cdef tuple chars_tuple = tuple(text_string)
    cdef int l = len(chars_tuple)
    cdef Texture old_target = renderer.target




    if text_box_rect is None:

        view_rect.x = 0
        view_rect.y = 0
        view_rect.w = destination_target_tex.width
        view_rect.h = destination_target_tex.height

    else:

        view_rect.x = text_box_rect.r.x
        view_rect.y = text_box_rect.r.y
        view_rect.w = text_box_rect.r.w
        view_rect.h = text_box_rect.r.h
   
    cdstrect.x = view_rect.x
    cdstrect.y = view_rect.y
    cdstrect.w = 0
    cdstrect.h = 0

    renderer.target = destination_target_tex
    
    if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
        renderer.target = old_target
        return

    for i in range(l):
        
        character = <str><object>PyTuple_GET_ITEM(chars_tuple, i)

        if character == "\n":
            cdstrect.y += line_spacing
            cdstrect.x = view_rect.x
            if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
                break
            
            continue
        
        if not PyDict_Contains(<dict>char_rect_dict, character):
            character = UNKNOWN_CHAR
        
        char_rect = <Rect><object>PyDict_GetItem(<dict>char_rect_dict, character)
        csrcrect_ptr = &char_rect.r
        
        cdstrect.w = csrcrect_ptr.w
        cdstrect.h = csrcrect_ptr.h

        res = SDL_RenderCopy(renderer._renderer, char_atlas_tex._tex, csrcrect_ptr, cdstrect_ptr)
        if res < 0:
            raise error()

        cdstrect.x += cdstrect.w
        if cdstrect.x+cdstrect.w > view_rect.w:
            cdstrect.y += line_spacing
            cdstrect.x = view_rect.x

        if cdstrect.y+line_spacing > view_rect.y+view_rect.h:
            break

    renderer.target = old_target


#cdef inline double random():
#    return cyrandom()
#
#
#cdef inline float lerpf(float a, float b, float factor):
#    return (1.0-fac)*a+fac*b
#
#
#cdef struct Particle2:
#
#    #Particle2* next
#    #Particle2* previous
#
#    Uint8 start_color[4]
#    Uint8 end_color[4]
#
#    SDL_BlendMode blendmode
#
#    SDL_FPoint pos
#    SDL_FPoint vel
#    SDL_FPoint start_scale, end_scale
#
#    float angle
#    float angular_vel
#    
#    float lifetime, current_lifetime
#
#    bint active
#    bint emitted
#
#    SDL_Rect* srcrect
#    SDL_FRect dstfrect
#
#
#
#
#cdef class ParticleSystem:
#    
#    #cdef Particle2* alive_head
#
#    #cdef Particle2* dead_head
#
#    cdef Particle2* particles
#
#
#    
#    cdef int particle_count
#    cdef int emitted_particle_count
#
#    cdef public Rect emit_area
#
#    cdef list source_rects
#    
#    cdef SDL_FPoint gravity
#    
#    cdef SDL_FPoint emit_direction
#
#    cdef float duration
#    cdef int particle_blendmode
# 
#    cdef Uint8 start_color[4]
#    cdef Uint8 end_color[4]
#
#    cdef float emit_arc
#    cdef float start_scale_range[2]
#    cdef float end_scale_range[2]
#
#    cdef float start_speed_range[2]
#    cdef float end_speed_range[2]
#
#
#
#    def __init__(self, Rect emit_area, int particle_count=10, float duration=1.0, float emit_arc=90.0, emit_direction=(1.0, 0.0), gravity=(0.0, 0.0), start_speed_range=(1.0, 1.0), end_speed_range=(1.0, 1.0),  start_scale_range=(1.0, 1.0), end_scale_range=(0.0,0.0), start_color=(255, 255, 255, 0), end_color=(255, 255, 255, 255), int particle_blendmode=0):
#
#
#        self.emit_area = emit_area
#        self.emit_arc = emit_arc
#
#        self.particle_count = particle_count
#        self.duration = duration
#
#        self.gravity.x = <float>gravity[0]
#        self.gravity.y = <float>gravity[1]
#
#        self.emit_direction.x = <float>emit_direction[0]
#        self.emit_direction.y = <float>emit_direction[1]
#
#        self.start_speed_range[0] = <float>start_speed_range[0]
#        self.start_speed_range[1] = <float>start_speed_range[1]
#        
#        self.end_speed_range[0] = <float>end_speed_range[0]
#        self.end_speed_range[1] = <float>end_speed_range[1]
#
#        self.start_scale_range[0] = <float>start_scale_range[0]
#        self.start_scale_range[1] = <float>start_scale_range[1]
#        
#        self.end_scale_range[0] = <float>end_scale_range[0]
#        self.end_scale_range[1] = <float>end_scale_range[1]
#
#        self.setup_particles()
#
#        
#    cdef void emit_particle(Particle2 p):
#
#        cdef Py_ssize_t i
#        cdef flaot
#
#        cdef SDL_Rect* emit_area = &self.emit_area.r
#
#        for i in range(4):
#            p.start_color[i] = self.start_color[i]
#            p.end_color[i] = self.end_color[i]
#        
#        p.start_scale.x = lerpf(self.start_scale_range[0], self.start_scale_range[1], <float>random())
#        p.start_scale.y = p.start_scale.x
#
#        p.end_scale.x = lerpf(self.end_scale_range[0], self.end_scale_range[1], <float>random())
#        p.end_scale.y = p.end_scale.x
#
#        p.pos.x = lerpf(emit_area.x, emit_area.x+emit_area.w, <float>random())
#        p.pos.y = lerpf(emit_area.y, emit_area.y+emit_area.h, <float>random())
#
#        p.vel.x = lerpf()
#        
#    
#    cdef int setup_particles() except NULL:
#
#        cdef Particle2* p = NULL
#        
#       self.particles = <Particle2*>PyMem_Malloc(sizeof(Particle2)*self.particle_count)
#
#       if not self.particles:
#           raise MemoryError()            
#
#       for i in range(particle_count):
#            p = self.particles[i]
#
#            p.active = 1
#            p.emitted = 0
#
#
#
#
#    cpdef update(float dt):
#
#        pass#cdef int to_emit = 
#
#
#
#
#    def __dealloc__(self):
#
#        PyMem_Free(self.particles)


        



        


            














    
    