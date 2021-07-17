let key_callback window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
  | _ -> ()

let () =
  GLFW.init ();
  GLFW.windowHint GLFW.ClientApi GLFW.OpenGLESApi;
  GLFW.windowHint GLFW.ContextVersionMajor 2;
  GLFW.windowHint GLFW.ContextVersionMinor 0;
  GLFW.windowHint GLFW.Resizable false;
  let window = GLFW.createWindow 800 450 "Peril" () in
  GLFW.makeContextCurrent (Some window);
  GLFW.setKeyCallback window (Some key_callback) |> ignore;
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    GLFW.swapBuffers window
  done;
  GLFW.destroyWindow window;
  GLFW.terminate ()
