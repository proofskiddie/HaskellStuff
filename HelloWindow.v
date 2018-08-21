(* Main *)

(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Converted imports: *)

Require Data.Foldable.
Require Data.StateVar.
Require GHC.Base.
Require GHC.Num.
Require Graphics.GL.Types.
Require Graphics.Rendering.OpenGL.GL.BeginEnd.
Require Graphics.Rendering.OpenGL.GL.Framebuffer.
Require Graphics.Rendering.OpenGL.GL.PrimitiveMode.
Require Graphics.Rendering.OpenGL.GL.Tensor.
Require Graphics.Rendering.OpenGL.GL.VertexSpec.
Require Graphics.UI.GLUT.Begin.
Require Graphics.UI.GLUT.Callbacks.Window.
Require Graphics.UI.GLUT.Initialization.
Require Graphics.UI.GLUT.Window.
Import Data.StateVar.Notations.
Import GHC.Base.Notations.

(* No type declarations to convert. *)
(* Converted value declarations: *)

Definition myPoints : list (Graphics.GL.Types.GLfloat *
                           Graphics.GL.Types.GLfloat * Graphics.GL.Types.GLfloat)%type :=
  cons (pair (pair (GHC.Num.negate (fromRational (Q.Qmake 1 4))) (fromRational
                   (Q.Qmake 1 4))) (GHC.Num.negate (fromRational (Q.Qmake 0 1)))) (cons (pair (pair
                                                                                              (fromRational (Q.Qmake 3
                                                                                                                     4))
                                                                                              (fromRational (Q.Qmake 7
                                                                                                                     20)))
                                                                                              (fromRational (Q.Qmake 0
                                                                                                                     1)))
                                                                                        (cons (pair (pair (fromRational
                                                                                                          (Q.Qmake 3 4))
                                                                                                          (GHC.Num.negate
                                                                                                          (fromRational
                                                                                                          (Q.Qmake 3
                                                                                                                   20))))
                                                                                                    (fromRational
                                                                                                    (Q.Qmake 0 1)))
                                                                                              nil)).

Definition displayPoints :=
  Graphics.Rendering.OpenGL.GL.Framebuffer.clear (cons
                                                 Graphics.Rendering.OpenGL.GL.Framebuffer.ColorBuffer nil) GHC.Base.>>
  (Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive
  Graphics.Rendering.OpenGL.GL.PrimitiveMode.Points GHC.Base.$ Data.Foldable.mapM_
  (fun arg_0__ =>
    match arg_0__ with
      | pair (pair x y) z => Graphics.Rendering.OpenGL.GL.VertexSpec.vertex GHC.Base.$
                             Graphics.Rendering.OpenGL.GL.Tensor.Vertex3 x y z
    end) myPoints).

Definition main :=
  Graphics.UI.GLUT.Initialization.getArgsAndInitialize GHC.Base.>>
  (Graphics.UI.GLUT.Window.createWindow (GHC.Base.hs_string__ "Hello Window")
  GHC.Base.>> ((Graphics.UI.GLUT.Callbacks.Window.displayCallback Data.StateVar.$=
  displayPoints) GHC.Base.>> Graphics.UI.GLUT.Begin.mainLoop)).

(* Unbound variables:
     Q.Qmake cons fromRational list nil op_zt__ pair Data.Foldable.mapM_
     Data.StateVar.op_zdze__ GHC.Base.op_zd__ GHC.Base.op_zgzg__ GHC.Num.negate
     Graphics.GL.Types.GLfloat Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive
     Graphics.Rendering.OpenGL.GL.Framebuffer.ColorBuffer
     Graphics.Rendering.OpenGL.GL.Framebuffer.clear
     Graphics.Rendering.OpenGL.GL.PrimitiveMode.Points
     Graphics.Rendering.OpenGL.GL.Tensor.Vertex3
     Graphics.Rendering.OpenGL.GL.VertexSpec.vertex Graphics.UI.GLUT.Begin.mainLoop
     Graphics.UI.GLUT.Callbacks.Window.displayCallback
     Graphics.UI.GLUT.Initialization.getArgsAndInitialize
     Graphics.UI.GLUT.Window.createWindow
*)
