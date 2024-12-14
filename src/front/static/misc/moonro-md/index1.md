## New SML web application framework


[slick&moonro](https://github.com/mnegovanovic/slick-moonro) is new web application framework for SML.
It is amalgamation of various projects that can be found on the internet. We are utilizing
[LunarML](https://github.com/minoki/LunarML) to integrate with [OpenResty](https://openresty.org)
on the back-end, with [SMLToJS](https://github.com/melsman/mlkit/blob/master/README_SMLTOJS.md)
on the front-end. slick&moonro is known to work on `x86_64 Linux` but it should be possible to
run compiled application on any platform for which OpenResty has support (its just Lua and JS after
all).

## Getting started <a id="_moonro_getting_started"></a>

Check out the source code [here](https://github.com/mnegovanovic/slick-moonro) and run it:

    $ git clone git@github.com:mnegovanovic/slick-moonro.git
    $ cd slick-moonro
    $ make run

Visit http://localhost:8080 (this all assumes you have working LunarML and SMLToJS install.
As well, you will need [tailwindcss-extra-linux-x64](https://github.com/dobicinaitis/tailwind-cli-extra/releases).)

## Structure & Architecture <a id="_moonro_structure"></a>

Lets have a look at top level directories and files:

    ├── install_local_rocks
    ├── lua_modules
    ├── Makefile
    ├── mime.types
    ├── nginx.conf
    ├── src
    │   ├── back
    │   │   ├── back.mlb
    │   │   ├── back.sml
    │   │   ├── dispatch.sml
    │   │   └── millet.toml
    │   ├── dist
    │   │   ├── moonro.sml
    │   │   ├── slick.sml
    │   │   ├── sml-json
    │   │   ├── sml-md5
    │   │   └── sml-setmap
    │   ├── front
    │   │   ├── front.mlb
    │   │   ├── front.sml
    │   │   ├── static
    │   │   └── tailwind.config.js
    │   └── shared
    └── watchd

`install_local_rocks`, `lua_modules` these are Lua libraries we find useful. Convenience bash
script.

`Makefile` contains some useful build targets, check it out now as you might need to change this
file to suit your project needs.

`mime.types`, `nginx.conf` files needed for OpenResty.

`src/back` Slick (back-end) SML application code. LunarML. (tweak and edit here)

`src/front` Moonro (front-end) SML application code. SMLToJS. (tweak and edit here)

`src/front/static` static assets. Contents of this directory are verbatim copied to build directory
on compilation.

`src/shared` SML application code shared by Slick and Moonro. (tweak and edit here)

`src/dist` distribution SML code. (do not edit, it might change between releases)

`watchd` is source code changes watch script.

On build each of mentioned components gets processed and important artifacts are copied to
`www` directory. From there we can run the application straight out of project source or
we can deploy it further.

## Hello world <a id="_moonro_hello_world"></a>

OK lets proceed with obligatory "hello world" example. Its pretty straight forward (you can find
the code in `src/back/back.sml`):
      
    val _ = S.action ("/hello_world", (fn (S.Request req) =>
        let in
            S.setResponseBody (S.Request req) "Hello world from Slick&Moonro!";
            S.setContentType (S.Request req) "text/html";
            S.flushRequest (S.Request req)
        end
    ))

We define an __action__ with route pattern and handler function. In this example handler
returns simple greeting. Have a look at `src/dist/slick.sml` for all the options.

## TODO app (SML in the browser) <a id="_moonro_todo_app"></a>

Bit more involved example, this time TODO app. We venture into front-end where SMLToJS is our
friend (code in `src/front/front.sml`):

    datatype todo = Todo of {
        id:     int option ref,
        text:   string option ref,
        done:   bool option ref,
        created:string option ref}

    fun newTodo t =
        Todo {
            id = ref NONE,
            text = ref (SOME t),
            done = ref (SOME false),
            created = ref (SOME (Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))))}

    val todos = ref [newTodo "Learn some SML", newTodo "Dinner", newTodo "Do the dishes"]

    fun mkTodoList_ () =
        let
            val e = taga0 "div" [("id", "todo_list"), ("class", "row todo-list")]
        in
            e
        end
    fun mkTodoList__ (r, e, p) =
        let
            fun mkList_ [] elements = elements
                | mkList_ (Todo t::ts) elements =
                    let
                        val text = if (valOf (!(#done t))) then
                                tag "b" (tag "s" ($(valOf (!(#text t)))))
                            else
                                tag "b" ($(valOf (!(#text t))))

                        fun done_ () =
                            let in
                                (#done t) := SOME (not (valOf (!(#done t))));
                                M.reshow "todo_list_c";
                                true
                            end
                        val button_done = taga "button" [("class", "btn btn-outline btn-primary")] ($"done")
                        val _ = Js.installEventHandler button_done Js.onclick done_

                        fun delete_ () =
                            let in
                                todos := List.filter (fn (Todo x) => ((valOf (!(#text t))) <> (valOf (!(#text x))))) (!todos);
                                M.reshow "todo_list_c";
                                true
                            end
                        val button_delete = taga "button" [("class", "btn btn-outline btn-primary")] ($"delete")
                        val _ = Js.installEventHandler button_delete Js.onclick delete_
                    in
                        mkList_ ts (elements & (text & button_done & button_delete & tag0 "br"))
                    end

            val todo_e = taga "div" [("id", "todo_list"), ("class", "row todo-list")]
              (taga "div" [("class", "col")] (mkList_ (!todos) ($"")))
        in
            SOME todo_e
        end
    val todo_list_c = M.mkComp mkTodoList_ mkTodoList__ (SOME "todo_list_c")

    fun mkTodoNew_ () =
        let
            val e = taga0 "div" [("id", "todo_new"), ("class", "row todo-new")]
        in
            e
        end
    fun mkTodoNew__ (r, e, p) =
            let 
                val todo_new_e = taga "textarea" [("id", "todo_new_txt")] ($"")
                fun keypress_ () =
                    let
                        val todo_new = newTodo (Js.value todo_new_e)
                    in
                        todos := (!todos) @ [todo_new];
                        M.reshow "todo_list_c";
                        false
                    end
                val _ = M.instalKeypressEventHandler todo_new_e 13 keypress_

                val todo_new_btn_e = taga "button" [("id", "todo_new_btn"), ("class", "btn btn-outline btn-primary")] ($"new")
                fun click_ () =
                    let
                        (*val todo_new = newTodo (J.getProperty (toForeignPtr todo_new_e) J.string "value")*)
                        val todo_new = newTodo (Js.value todo_new_e)
                    in
                        todos := (!todos) @ [todo_new];
                        M.reshow "todo_list_c";
                        true
                    end
                val _ = Js.installEventHandler todo_new_btn_e Js.onclick click_
            in
                SOME (taga "div" [("id", "todo_new"), ("class", "row todo-new")]
                  (taga "div" [("class", "col")] (todo_new_e & (tag0 "br") & todo_new_btn_e & (tag0 "br") & (tag0 "br"))))
            end
    val todo_new_c = M.mkComp mkTodoNew_ mkTodoNew__ NONE
    val _ = M.mkPage "/moonro" [top_menu_c, moonro_toc_c, moonro1_c, todo_list_c, todo_new_c, moonro2_c] NONE

Moonro is simple front-end framework. In it, web application consists of pages and each page
is made up of components. Its best to read the source code at `src/dist/moonro.sml`.
Special attention to `Moonro.mkComp` and `Moonro.mkPage` functions as you will be using them:

`mkComp` takes 3 arguments: `onLoad` function, `onShow` function, optional ID (if you do not supply
one random one will be generated).
    
    fun mkComp (onLoad: unit -> Js.elem) (onShow: (request * Js.elem * Js.elem) -> Js.elem option) (id: string option) =
        ...

`mkPage` takes 3 arguments: URL pattern, component list, optional ID

    fun mkPage (pattern: string) (cs: component list) (id: string option) =
        ...

## TODO

