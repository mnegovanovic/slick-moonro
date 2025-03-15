structure Moonro =
struct

    structure J = JsCore
    structure O = JsCore.Object
    structure A = JsCore.Array
    open Js.Element infix &
    open SMUtils

    datatype request = Request of {
        request_uri:    string,
        path_info:      string,
        query_string:   string,
        path_args:      (string * string) list ref
    }

    datatype component = Comp of {
        id: string,
        e: Js.elem option ref,
        parent: Js.elem option ref,
        onLoad: unit -> Js.elem option,
        onShow: (request * Js.elem * Js.elem) -> Js.elem option
        }
    
    datatype page = Page of {
        id: string,
        title: string,
        pattern: string,
        cs: component list,
        props: (string * string) list
        }

    val pages: (string list * page) list ref = ref []
    val components: (string * component) list ref = ref []
    val fragments: (string*string) list ref = ref []
    val fe_session: FESession.fe_session ref = ref []

    fun copyComp (Comp c: component): component =
        let
            val c1 = Comp {
                id = #id c,
                e = ref NONE,
                parent = ref NONE,
                onLoad = #onLoad c,
                onShow = #onShow c}
        in
            c1
        end

    fun log s = J.call1 ("console.log", J.string, J.unit) s

    fun encode (s: string): string =
        let
            val arg1 = ("s", J.string)
            val stmt = "return utf8.encode(s);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.string} (s)
        end

    fun decode (s: string): string =
        let
            val arg1 = ("s", J.string)
            val stmt = "return utf8.decode(s);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.string} (s)
        end

    fun md5hex (s: string): string =
        let
            val arg1 = ("s", J.string)
            val stmt = "return md5(s);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.string} (s)
        end

    fun uuidv4 (): string =
        let
            val stmt = "return window.crypto.randomUUID();"
        in
            J.exec0 {stmt=stmt, res=J.string} ()
        end
    
    val random_md_5_counter_ = ref 0
    fun randomMD5 (): string =
        let
            val _ = random_md_5_counter_ := !random_md_5_counter_ + 1
        in
            md5hex ((Int.toString (!random_md_5_counter_))^uuidv4 ()^(LargeInt.toString (Time.toMilliseconds (Time.now ()))))
        end

    fun regPage (Page p): unit =
        let
            val pattern = #pattern p
            val pparts = String.fields (fn c => c = #"/") pattern
            val pparts = List.filter (fn p => p <> "") pparts
            val cs = #cs p
        in
            pages := (pparts, Page p) :: (!pages);
            List.app (fn (Comp c) => components := (#id c, Comp c)::(!components)) cs
        end
   
    fun jqGET (url: string) (data: string) (cbfn: string -> unit) =
         let
            val arg1 = ("u", J.string)
            val arg2 = ("d", J.string)
            val arg3 = ("f", J.==>(J.string,J.unit))
            val stmt = "$.get(u, d, f);"
        in
            J.exec3 {stmt=stmt, arg1=arg1, arg2=arg2, arg3=arg3, res=J.string} (url, data, cbfn)
        end
     
     fun jqPOST (url: string) (data: string) (cbfn: string -> unit) =
         let
            val arg1 = ("u", J.string)
            val arg2 = ("d", J.string)
            val arg3 = ("f", J.==>(J.string,J.unit))
            val stmt = "$.post(u, d, f);"
        in
            J.exec3 {stmt=stmt, arg1=arg1, arg2=arg2, arg3=arg3, res=J.string} (url, data, cbfn)
        end

    fun mdParse (s: string): string =
        let
            val arg1 = ("s", J.string)
            val stmt = "return marked.parse(s);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.string} (s)
        end

    fun replaceState (state: (string*string) list) (p: string): unit =
        let
            val s = O.fromList J.string state
            val arg1 = ("s", J.fptr)
            val arg2 = ("p", J.string)
            val stmt = "window.history.replaceState(s, '', p);"
        in
            J.exec2 {stmt=stmt, arg1=arg1, arg2=arg2, res=J.unit} (s, p)
        end

    fun pushState (state: (string*string) list) (p: string): unit =
        let
            val s = O.fromList J.string state
            val arg1 = ("s", J.fptr)
            val arg2 = ("p", J.string)
            val stmt = "window.history.pushState(s, '', p);"
        in
            J.exec2 {stmt=stmt, arg1=arg1, arg2=arg2, res=J.unit} (s, p)
        end

    fun hide (selector: string): unit =
        let
            val stmt = "$('"^selector^"').hide();"
        in
            J.exec0 {stmt=stmt, res=J.fptr} ();
            ()
        end

    fun show (selector: string): unit =
        let
            val stmt = "$('"^selector^"').show();"
        in
            J.exec0 {stmt=stmt, res=J.fptr} ();
            ()
        end
    
    fun showHideWithTimer (selector: string) (i: int): unit =
        let in
            show selector;
            Js.setTimeout i (fn () => hide selector);
            ()
        end
    
    fun addClass selector class =
        let
            val stmt = "$('"^selector^"').addClass('"^class^"');"
        in
            J.exec0 {stmt=stmt, res=J.fptr} ()
        end
    
    fun removeClass selector class =
        let
            val stmt = "$('"^selector^"').removeClass('"^class^"');"
        in
            J.exec0 {stmt=stmt, res=J.fptr} ()
        end
    
    fun getEBI (id: string): Js.elem =
        let
            val id = if (String.isPrefix "#" id) then String.extract (id, 1, NONE) else id
        in
            case Js.getElementById Js.document id of
                SOME e => e
                | NONE => raise Fail ("getEBI() Missing id in document: " ^ id)
        end
    
    fun onDocumentReady (f: unit -> unit): unit =
        let
            val arg1 = ("f", J.==>(J.unit,J.unit))
            val stmt = "addEventListener(\"DOMContentLoaded\", f);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.unit} (f)
        end

    fun installKeypressEventHandler (e: Js.elem) (kc: int) (f: unit -> bool) : unit =
        let
            val arg1 = ("e", J.fptr)
            val arg2 = ("kc", J.int)
            val arg3 = ("f", J.==>(J.unit,J.bool))
            val stmt = "e.onkeypress = function (ev) { if (kc == -1 || ev.keyCode == kc) {return f(); } };"
        in
            J.exec3 {stmt=stmt, arg1=arg1, arg2=arg2, arg3=arg3, res=J.unit} (toForeignPtr e, kc, f)
        end

   fun showComp (Request r) (Comp cmp) =
        let
            val onShowFN = #onShow cmp
            val e_current = valOf (! (#e cmp))
            val target = valOf (! (#parent cmp))
        in
            case onShowFN (Request r, e_current, target) of
                SOME e =>
                    let in
                        Js.replaceChild target e e_current;
                        (#e cmp) := SOME e;
                        (*(#parent cmp) := SOME target;*)
                        ()
                    end
                | NONE => ();
            ()
        end

    fun setCurrentTitle (title: string): unit =
        let
            val stmt1 = "$('#head_id title').remove();"
            val stmt2 = "$('#head_id').append('<title>"^title^"</title>');"
        in
            J.exec0 {stmt=stmt1, res=J.unit} ();
            J.exec0 {stmt=stmt2, res=J.unit} ()
        end

    fun showPage (Page pg) (Request r) (do_push_state: bool): unit =
        let
            val id = #id pg
            val title = #title pg
            val cs = #cs pg
            
            val path_info = #path_info r
            val query_string = #query_string r
            val query_string = case query_string of
                "" => ""
                | _ => "?"^query_string

            fun showComp_ (Request r) (Comp c) =
                let in
                    showComp (Request r) (Comp c)
                end handle
                    exc => let
                            val target = valOf (! (#e c))
                            val msg = "Moonro.showPage.showComp_() exception "^(exnName exc)^" "^(exnMessage exc)^" component ID: "^(#id c)
                            val err_e = taga "div" [("style", "color: red; font-size: 2em;")] ($(msg))
                        in
                            log msg;
                            Js.appendChild target err_e
                        end

            fun showPage_ () =
                let in
                    setCurrentTitle title;
                    List.app (showComp_ (Request r)) cs;
                    hide ".pg-container";
                    show ("#"^id);
                    if do_push_state then
                        pushState [] ("#"^path_info^query_string)
                    else
                        ()
                end

            fun onSession_ data =
                let in
                    fe_session := FESession.load data;
                    showPage_ ()
                end 
        in
            jqGET "/sm/fe_session_get" "" onSession_;
            ()
        end

    fun matchPattern (pparts: string list): (page * (string * string) list) option =
        let
            fun mm [] [] (args: (string * string) list): bool * ((string * string) list) = (true, args)
                | mm route [] args = (false, [])
                | mm [] request args = (false, [])
                | mm (route_part::route_tl) (request_part::request_tl) args =
                    if (List.length route_tl) <> (List.length request_tl) then
                        (false, [])
                    else if (route_part <> request_part) andalso not ((String.isPrefix "[" route_part) andalso (String.isSuffix "]" route_part)) then
                        (false, [])
                    else
                        if (String.isPrefix "[" route_part) andalso (String.isSuffix "]" route_part) then
                            let
                                val path_arg = String.extract (route_part, 1, NONE)
                                val path_arg = String.extract (path_arg, 0, SOME((String.size path_arg) - 1))
                                val path_arg_val = request_part
                            in
                                mm route_tl request_tl ((path_arg, path_arg_val) :: args)
                            end
                        else
                            mm route_tl request_tl args
            
            fun m [] pparts: (page * ((string * string) list)) option = NONE
                | m ((route_pparts, pg)::tl) pparts =
                    let
                        val (success, path_args) = mm route_pparts pparts []
                    in
                        if success then
                            SOME (pg, path_args)
                        else
                            m tl pparts
                    end
        in
            m (!pages) pparts
        end

    and routeRequest (Request r) (do_push_state: bool): unit =
        let
            val path = #path_info r
        in
            let
                val pparts = String.fields (fn c => c = #"/") path
                val pparts = List.filter (fn p => p <> "") pparts
            in
                case matchPattern pparts of
                    NONE => show404 ()
                    | SOME (pg, path_args) => 
                        let in
                            (#path_args r) := path_args;
                            showPage pg (Request r) do_push_state
                        end handle
                            exc => log ("Moonro.routeRequest() showPage() exception "^(exnName exc)^" "^(exnMessage exc))
            end
        end

    and navigate (path:string) (do_push_state: bool): unit =
        if String.isPrefix "#" path then
            let
                val pparts1 = String.fields (fn c => c = #"#") path
                val pparts1 = List.filter (fn p => p <> "") pparts1

                val all = List.nth (pparts1, 0)
                val pparts2 = String.fields (fn c => c = #"?") all
                val pparts2 = List.filter (fn p => p <> "") pparts2
            in
                case List.length pparts2 of
                    1 =>
                        let
                            val r = Request {
                                request_uri = "",
                                path_info = List.nth (pparts2, 0),
                                query_string = "",
                                path_args = ref []}
                        in
                            routeRequest r do_push_state
                        end
                    | 2 =>
                        let
                            val r = Request {
                                request_uri = "",
                                path_info = List.nth (pparts2, 0),
                                query_string = List.nth (pparts2, 1),
                                path_args = ref []}
                        in
                            routeRequest r do_push_state
                        end
                    | _ => show404 ()

           end
        else
            show404 ()
    
    and show404 (): unit =
        let in
            navigate "#/404-not-found" true;
            ()
        end
    
    fun scrollIntoView (id: string): unit =
        let
            val _ = getEBI id   (* we raise a Fail in case element is missing *)
            val arg1 = ("s", J.string)
            val stmt = "document.getElementById(s).scrollIntoView();"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.unit} (id)
        end

    fun popState () =
        let
            val href = J.exec0 {stmt="return window.location.href;", res=J.string} ()
            val pparts1 = String.fields (fn c => c = #"#") href
            val pparts1 = List.filter (fn p => p <> "") pparts1
        in
            case List.length pparts1 of
                2 =>
                    let
                        val target = List.nth (pparts1, 1)
                    in
                        if List.exists (fn (f_id, p_id) => f_id = ("#"^target)) (!fragments) then
                            let
                                val (f_id, p_id) = List.nth (List.filter (fn (f_id, p_id) => f_id = ("#"^target)) (!fragments), 0)
                            in
                                navigate p_id false;
                                scrollIntoView (String.extract (f_id, 1, NONE))
                            end
                        else
                            navigate ("#"^target) false
                    end
                | 1 => navigate "#/" true
                | _ => show404 ()
        end
    
    fun onPopState (f: unit -> unit): unit =
        let
            val arg1 = ("f", J.==>(J.unit,J.unit))
            val stmt = "addEventListener(\"popstate\", f);"
        in
            J.exec1 {stmt=stmt, arg1=arg1, res=J.unit} (f)
        end
  
    fun mkA text page_id class =
        let
            val e = taga "a" [("href", page_id), ("class", class)] ($(decode text))

            fun click_ () =
                let in
                    navigate page_id true;
                    false
                end
        in
            Js.installEventHandler e Js.onclick click_;
            e
        end
    
    fun mkF text fragment_id page_id class =
        let
            val e = taga "a" [("href", fragment_id), ("class", class)] ($(decode text))

            fun click_ () =
                let in
                    scrollIntoView (String.extract (fragment_id, 1, NONE));
                    pushState [] (fragment_id);
                    false
                end
        in
            fragments := (fragment_id, page_id)::(!fragments);
            Js.installEventHandler e Js.onclick click_;
            e
        end

    fun mkComp (onLoad: unit -> Js.elem option) (onShow: (request * Js.elem * Js.elem) -> Js.elem option) (id: string option) =
        let
            val id = case id of
                NONE => randomMD5 ()
                | SOME id => id
            
            val c = Comp {
                id = id,
                e = ref NONE,
                parent = ref NONE,
                onLoad = onLoad,
                onShow = onShow}
        in
            c
        end
    
    fun reshow (id: string) =
        let
            val cs = List.filter (fn (id1, c) => id = id1) (!components)
            fun reshowComp_ (id: string, Comp c: component) =
                let
                    val r = Request {
                        request_uri = "",
                        path_info = "",
                        query_string = "",
                        path_args = ref []}
                in
                    showComp r (Comp c)
                end
        in
            List.app reshowComp_ cs
        end

    fun mkPage (pattern: string) (cs: component list) (props: (string * string) list) =
        let
            val id = case (findPairValue "id" props) of
                NONE => randomMD5 ()
                | SOME id => id
            val title = case (findPairValue "title" props) of
                NONE => "Moonro page"
                | SOME title => title
            val comps = List.map copyComp cs
            
            val pg = Page {
                pattern = pattern,
                cs = comps,
                id = id,
                title = title,
                props = props}
        in
            regPage pg;
            pg
        end

    fun loadComp (target: Js.elem) (Comp cmp) =
        let
            val onLoadFN = #onLoad cmp
            val e = case onLoadFN () of
                NONE => tag "div" ($"onLoadFN() returned NONE, element placeholder")
                | SOME e => e
        in
            (#e cmp) := SOME e;
            (#parent cmp) := SOME target;
            Js.appendChild target e;
            ()
        end
    
    fun loadPages () =
        let
            val bdy = getEBI "body_id"
            
            fun loadDOM_ (ppath: string list, Page pg) =
                let
                    val page_id = #id pg
                    val props = #props pg
                    val page_class = case (findPairValue "page-class" props) of
                        NONE => "pg-container"
                        | SOME class => "pg-container "^class
                    val container_div = taga0 "div" [("id", page_id), ("class", page_class)]
                    val cs = #cs pg
                in
                    List.app (loadComp container_div) cs;
                    Js.appendChild bdy container_div
                end handle
                    exc => let
                            val msg = "Moonro.loadPages.loadDOM_() exception "^(exnName exc)^" "^(exnMessage exc)
                            val err_e = taga "div" [("style", "color: red; font-size: 2em;")] ($(msg))
                        in
                            log msg;
                            Js.appendChild bdy err_e
                        end
        in
            List.app loadDOM_ (!pages)
        end

    val _ = _export("navigate", fn (href: string) => navigate href true)
    fun elemFromString (s: string): Js.elem =
        let
            val arg1 = ("el_string", J.string)
            val stmt = "var el = $(el_string); $('a.page-link', el).on('click', function() {SMLtoJs.navigate ($(this).attr('href')); return false;}); return el[0];"
        in
            fromForeignPtr (J.exec1 {stmt=stmt, arg1=arg1, res=J.fptr} (s))
        end

    fun init (myInit: unit -> unit): unit =
        let
            fun mk404 () =
                let
                    val c_404 = mkComp
                        (fn () => SOME (taga "div" [("class", "row")] (tag "h1" ($"404 - Not Found"))))
                        (fn (r, e, p) => NONE)
                        NONE
                in
                    mkPage "/404-not-found" [c_404] [("title", "404 - Not Found")];
                    ()
                end

            val hd = getEBI "head_id"
            val meta_utf8 = taga0 "meta" [("charset", "UTF-8")]
            val meta_viewport = taga0 "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
            val _ = Js.appendChild hd meta_utf8
            val _ = Js.appendChild hd meta_viewport
            val _ = mk404 ()
            val nav = fn () => popState ()
        in
            onPopState popState;
            onDocumentReady loadPages;
            onDocumentReady myInit;
            onDocumentReady nav;
            ()
        end

end

