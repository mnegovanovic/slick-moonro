structure J = JsCore
structure O = JsCore.Object
structure A = JsCore.Array
open Js.Element infix &
structure M = Moonro
structure WA = WebAction

(*
 *   HELPER FUNCTIONS
 *)
fun serialize (selector: string): string =
    let
        val stmt = "return $('"^selector^" :input').serialize();"
        in
            J.exec0 {stmt=stmt, res=J.string} ()
        end

fun applyWebActionResult (selector: string) (WA.Result res: WA.result): unit =
    let
        val stmt = "$('.label-text', $('"^selector^"')).removeClass('text-red-500');"
        val _ = J.exec0 {stmt=stmt, res=J.unit} ()
        
        val msg = if (#ok res) then
                "<div role='alert' class='alert alert-success'><span>"^(#msg res)^"</span></div>"
            else
                ("<div role='alert' class='alert alert-error'><span>"^(#msg res)
                ^"<br />"^(List.foldl (fn ((k,v), acc)=>acc^k^" - "^v^"<br />") "" (#errs res))
                ^"</span></div>")
        val stmt = "$('.alert-container', $('"^selector^"')).html(\""^msg^"\").show();"
        val _ = J.exec0 {stmt=stmt, res=J.unit} ()
    in
        if (#ok res) then
            let
                val stmt = "$('input', $('"^selector^"')).val('');"
                val _ = J.exec0 {stmt=stmt, res=J.unit} ()
                val stmt = "$('textarea', $('"^selector^"')).val('');"
                val _ = J.exec0 {stmt=stmt, res=J.unit} ()
            in
                ()
            end
        else
            List.app (fn (k,v) =>
                let
                    val stmt = "$(\"[for='"^k^"']\", $('"^selector^"')).addClass('text-red-500');"
                in
                    J.exec0 {stmt=stmt, res=J.unit} ()
                end) (#errs res)
    end

(*
 *   TOP MENU
 *)
fun mkTopMenu_ () =
    let
        val e = taga "div" [("class", "navbar bg-base-100")] (
            (M.mkA "atreides-host" "#/" "btn btn-ghost text-xl")
            & (M.mkA "slick&moonro" "#/moonro" "btn btn-ghost text-base")
            (*& (M.mkA "more-content" "#/cms/index" "btn btn-ghost text-base")*)
        )
    in
        SOME e
    end
val top_menu_c = M.mkComp mkTopMenu_ (fn (r, e, p) => NONE) NONE

fun mkFooter_ () =
    let
        val e = taga "footer" [("class", "footer footer-center text-base-content p-4")] (
          tag "aside" (
            tag "p" ($"Copyright (c) 2024 - All rights reserved by Atreides Host"))
        )
    in
        SOME e
    end
val main_footer_c = M.mkComp mkFooter_ (fn (r, e, p) => NONE) NONE

(*
 * FRONT PAGE
 *)
fun mkAHost_ () =
    let
        val e = taga "div" [("class", "grid grid-cols-1 sm:grid-cols-2 grid-rows-2 gap-4")] (
            taga0 "img" [("class", "rounded"), ("src", "/static/img/laptop_unsplash.jpg"), ("alt", "laptop")]
            & taga0 "div" [("id", "a_host_1"), ("class", "text-base")]
            & taga0 "div" [("id", "a_host_2"), ("class", "text-base")]
            & taga0 "img" [("class", "rounded"), ("src", "/static/img/drops.jpeg"), ("alt", "drops")])
    in
        SOME e
    end
fun mkAHost__ (r, e, p) =
    let
        fun cbAHost1 html =
            Js.innerHTML (M.getEBI "a_host_1") (M.mdParse html)
        val _ = M.jqGET "/md/a-host-1" "" cbAHost1
        
        fun cbAHost2 html =
            Js.innerHTML (M.getEBI "a_host_2") (M.mdParse html)
        val _ = M.jqGET "/md/a-host-2" "" cbAHost2
    in
        NONE
    end
val a_host_c = M.mkComp mkAHost_ mkAHost__ NONE

fun mkContactForm_ () =
    let
        val e = taga "h2" [("class", "mt-8")] ($"Enquiry? Send us an email!") & taga "div" [("id", "contact_form_id"), ("class", "contact-form")] (
          taga "label" [("class", "form-control w-full max-w-xs")] (
            taga "div" [("class", "label")] (
              taga "span" [("class", "label-text"), ("for", "name")] ($"Name?"))
            & taga0 "input" [("type", "text"), ("name", "name"), ("placeholder", "Your name"), ("class", "input input-bordered w-full max-w-xs")])
          
          & taga "label" [("class", "form-control w-full max-w-xs")] (
            taga "div" [("class", "label")] (
              taga "span" [("class", "label-text"), ("for", "email")] ($"Email?"))
            & taga0 "input" [("type", "text"), ("name", "email"), ("placeholder", "Your email"), ("class", "input input-bordered w-full max-w-xs")])
          
          & taga "label" [("class", "form-control")] (
            taga "div" [("class", "label")] (
              taga "span" [("class", "label-text"), ("for", "msg")] ($"Message?"))
            & taga0 "textarea" [("class", "textarea textarea-bordered h-24"), ("name", "msg"), ("placeholder", "Your message")])
          
          & taga "button" [("id", "btn_submit"), ("class", "btn btn-outline mt-2")] ($"Submit")
          & taga0 "div" [("class", "mt-4 alert-container"), ("style", "display: none;")]
        )
    in
        SOME e
    end
fun mkContactForm__ (r, e, p) =
    let
        val btn = M.getEBI "btn_submit"
        fun submit_ () =
            let
                val post_body = serialize "#contact_form_id"
                fun cb_ data =
                    let
                        val (WA.Result res) = WA.load data
                    in
                        applyWebActionResult "#contact_form_id" (WA.Result res)
                    end
            in
                M.jqPOST "/register_enquiry" post_body cb_;
                false
            end
        val _ = Js.installEventHandler btn Js.onclick submit_
    in
        NONE
    end
val contact_form_c = M.mkComp mkContactForm_ mkContactForm__ NONE
val _ = M.mkPage "/" [top_menu_c,
                      a_host_c,
                      contact_form_c,
                      main_footer_c] [("title", "Atreides Host - Bespoke digital solutions for businesses"),
                                      ("page-class", "container mx-auto px-4")]

(*
 * MOONRO
 *)
fun mkMoonroTOC_ () =
    let
        val e = taga0 "div" [("id", "moonro_id_toc"), ("class", "content")]
    in
        Js.appendChild e (M.mkF "Getting started" "#_moonro_getting_started" "#/moonro" "fragment-link");
        Js.appendChild e (tag0 "br");
        Js.appendChild e (M.mkF "Structure & Architecture" "#_moonro_structure" "#/moonro" "fragment-link");
        Js.appendChild e (tag0 "br");
        Js.appendChild e (M.mkF "Hello world" "#_moonro_hello_world" "#/moonro" "fragment-link");
        Js.appendChild e (tag0 "br");
        Js.appendChild e (M.mkF "TODO app" "#_moonro_todo_app" "#/moonro" "fragment-link");
        SOME e
    end
val moonro_toc_c = M.mkComp mkMoonroTOC_ (fn (r, e, p) => NONE) NONE

fun mkMoonro1_ () =
    let
        val e = taga0 "div" [("id", "moonro_id_1"), ("class", "content")]
    in
        SOME e
    end
fun mkMoonro1__ (r, e, p) =
    let
        fun cb_ html =
            Js.innerHTML (M.getEBI "moonro_id_1") (M.mdParse html)
        val _ = M.jqGET "/md/moonro-index-1" "" cb_
    in
        SOME e
    end
val moonro1_c = M.mkComp mkMoonro1_ mkMoonro1__ NONE

fun mkMoonro2_ () =
    let
        val e = taga0 "div" [("id", "moonro_id_2"), ("class", "content")]
    in
        SOME e
    end
fun mkMoonro2__ (r, e, p) =
    let
        fun cb_ html =
            Js.innerHTML (M.getEBI "moonro_id_2") (M.mdParse html)
        val _ = M.jqGET "/md/moonro-index-2" "" cb_
    in
        SOME e
    end
val moonro2_c = M.mkComp mkMoonro2_ mkMoonro2__ NONE

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
        SOME e
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
        SOME e
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
            val _ = M.installKeypressEventHandler todo_new_e 13 keypress_

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
val _ = M.mkPage "/moonro" [top_menu_c,
                            moonro_toc_c,
                            moonro1_c,
                            todo_list_c,
                            todo_new_c,
                            moonro2_c,
                            main_footer_c] [("title", "Slick&Moonro - SML web application framework"),
                                            ("page-class", "container mx-auto px-4")]

(*
 * CMS
 *)
(*
fun mkCMS_ () =
    let
        val e = taga0 "div" [("id", "cms_target_id"), ("class", "cms-content")]
    in
        e
    end
fun mkCMS__ (M.Request r, e, p) =
    let
        val page = valOf (M.findPairValue "page" (!(#path_args r)))
        fun cb_ prf =
            let in
                if prf = "not-found" then
                    Js.innerHTML (M.getEBI "cms_target_id") "<h1 style=\"color: red; font-size: 2em;\">404 - Not Found</h1>"
                else
                    let
                        val [page, blocks] = Record.loadLString prf
                        val (Record.CmsPage page) = Record.load page
                        val blocks = Record.loadL blocks
                    in
                        ()
                    end;
                ()
            end
        val _ = M.jqGET ("/_cms/"^page) "" cb_
        (*val stmt = "$(document.body).on('click', '.cms-a', function() { alert($(this).val()); });"
        J.exec0 {stmt=stmt, res=J.unit} ()*)
    in
        NONE
    end
val cms_c = M.mkComp mkCMS_ mkCMS__ NONE
val _ = M.mkPage "/cms/[page]" [cms_c] (SOME "cms")
*)

(*
 * GENERAL
 *)
fun myInit_ () =
    ()
    (*
    (M.addClass "#body_id" "bg-dark";
    M.addClass "#body_id" "text-light";
    ())*)

fun main () = M.init (myInit_)
val _ = main ()

