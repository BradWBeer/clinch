# Clinch


## Overview

Clinch is a simple, yet powerful 3d game engine for Lisp. 

Eventually Clinch will become a stable and fast workhorse tool for developing games, visualizations and productivity software. I have plans for a graphical shell which incorporates the strengths of Lisp, 3D, shaders, 2d vector graphics, richtext support, 3d physics and much more.

### Clinch Extensions:

* A suite of modern 3d and 2d tools through OpenGL

* Access to commercial grade windowing, controller, force-feedback, sound, music and more thorough SDL2

* 3d physics with joints and motors with the ODE physics engine

* Ability to live code with Lisp.

* Texture loading and saving with FreeImage

* Integration with 2D vector graphics with Cairo

* Integration with fonts and text positioning with Pango

* 3D asset importing using ClassImp (Coming Soon!)

## Design Goals

### Fast 

Clinch should be as fast or faster than most script-based graphics engines and require less development time. While it may never rival professional libraries and engines, the ability to modify the 3D engine and environment from the REPL allows skilled developers to create applications in a fast, intuitive and flexible way.

### Simple

Clinch should be as simple as possible for someone familiar with 3D graphics programming to understand. I still remember how easy it was to write a single pixel to the screen in DOS and while I can't simplify to that degree (without losing modern power and flexibility), I can remove many of the most common difficulties. These include texture and vertex buffers, shader compiling and linking, shader variable passing, drawing text and 2D graphics, pipelines and 3D transformations. 

### Intuitive

Time spent looking for solutions is spent time, period. It ruins developer flow and can stop a project (especially a small one) indefinitely. Clinch should have few basic primitives which solve the general cases well and allow for easy replacements when necessary. It should be well documented and have a consistent interface. I will strive to keep abstraction leaks at a minimum. 

### Flexible

Clinch does not inflict any particular design onto its client applications. It can be easily modified to create a specific engine. It uses plugins to minimize it's own overhead.

## Architecture

Although Clinch can be used as a complete engine, most parts of Clinch are independent. You can use objects by themselves as best suits your application. For example, you can use a buffer object by itself. This also helps with testing by isolating the various parts of Clinch. The following is in hierarchical order based on the default configuration. This is to explain it as clearly as possible, not to indicate a necessary design. 

### Transforms

A transform is a 4x4 matrix which is used to hold and apply a C array of 16 floating values. CL-game-math is used as the default linear algebra library and its arrays may be passed to the shader. There are funcions for creating various projection matrices.

### Nodes

Nodes are not usually the topmost objects in the hierarchy, however they are the most important. Nodes abstract 3D transform "chains" and hierarchy. Nodes encapulate transforms and can be scaled, rotated, and translated. They also may have children. Nodes are multiplied together in a hierarchy to create an effective transform which it stores in a hash by a list of all its parents. This allows a node to be reused in any way necessary. A node can be used in multiple places within a hierarchy (or tree) or even in several different ones. This is done by passing a list of parents to the update function. It will then append itself and call update on its children. In this way, the transforms are only calculated when they or their parent(s) have changed. 

### Buffers

Buffers abstract the shared data on the graphics card. They can carry almost any data to the graphics card in bulk including vertexes, normals, and texture coordinates. Buffers are usually sent to a shader using the entity object. A buffer's data can be accessed directly by using the map/unmap functions, or more simply using with-mapped-buffer.

### Textures

Textures abstract the 2D textures and allow easy access to their raw data. Textures can be loaded from files, drawn on with vector graphics or used as render targets. They use Pixel Buffer Object to make reading and writing faster.

### Shaders

Shaders are the compiled output from text-based vertex and fragment shaders. They require a name, vertex shader source code, fragment shader source code, a list of uniforms and a list of attributes. Vertices and index buffers need not be specified if you are not using them. 

### Entities

Entities are the rendered items. They bring together the shader, buffers, textures, attributes and uniforms and transforms together into something which can be rendered on the screen. A shader and a (currently) an index buffer is required although they would be useless without at least one vertex buffer. Each member of the VALUES slot can be either an :attribute, :uniform or :vertices. 

### Viewports

An application can have several viewports. As Clinch does not force any particular windowing implementation, it can not have a window class. Once your window is set up, however, you can use the viewport handle for drawing an area on screen. The most common children the cameras, which will be rendered in order unless then are not enabled.
