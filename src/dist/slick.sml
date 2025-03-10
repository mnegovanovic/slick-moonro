structure Slick = struct

    open SMUtils
    datatype response = Response of {
        code:           int ref,
        body:           string ref,
        redirect:       string option ref,
        headers:        (string * string) list ref
    }

    datatype request = Request of {
        scheme:         string,
        host:           string,
        uri:            string,
        path:           string,
        query:          string option,
        path_args:      (string * string) list ref,
        handler:        (request -> unit) option ref,
        session:        Lua.value option ref,
        mysql_db:       Lua.value option ref,
        mailer:         Lua.value option ref,
        response:       response
    }
    type action_fn = request -> unit

    val actions: (string list * action_fn) list ref = ref []
    
    fun stringListToLuaArray_ (l: string list): Lua.value =
        let
            val (_, res) = List.foldl (fn (x, (i, t)) => (Lua.set (t, Lua.fromInt i, Lua.fromString x); (i+1, t))) (1, Lua.newTable()) l
        in
            res
        end
    
    exception FileRead of string
    fun readFile (path: string): string =
        let
            val open_ = Lua.field (Lua.global "io", "open")
            val f = Lua.call1 open_ #[Lua.fromString path, Lua.fromString "rb"]
        in
            if Lua.isFalsy f then
                raise FileRead "File open() failed"
            else
                let
                    val content = Lua.unsafeFromValue (Lua.method1 (f, "read") #[Lua.fromString "*a"]) : string
                    val _ = Lua.method0 (f, "close") #[]
                in
                    content
                end
        end
    
    exception FileWrite of string
    fun writeFile (path: string) (content: string): unit =
        let
            val open_ = Lua.field (Lua.global "io", "open")
            val f = Lua.call1 open_ #[Lua.fromString path, Lua.fromString "w"]
        in
            if Lua.isFalsy f then
                raise FileWrite "File open() failed"
            else
                let in
                    Lua.method0 (f, "write") #[Lua.fromString content];
                    Lua.method0 (f, "flush") #[];
                    Lua.method0 (f, "close") #[]
                end
        end

    fun ee (s: string) =
        let
            val ee_ = Lua.field (Lua.global "ngx", "quote_sql_str")
            val s_escaped = Lua.call1 ee_ #[Lua.fromString s]
        in
            Lua.unsafeFromValue s_escaped : string
        end
    
    fun action (pattern, afn) =
        let
            val pparts = String.fields (fn c => c = #"/") pattern
            val pparts = List.filter (fn p => p <> "") pparts
        in
            actions := (pparts, afn) :: (!actions)
        end


    fun say (s: string): unit =
        let
            val say_ = Lua.field (Lua.global "ngx", "say")
        in
            Lua.call0 say_ #[Lua.fromString s]
        end

    fun info (s: string): unit =
        let
            val log_ = Lua.field (Lua.global "ngx", "log")
            val INFO_ = Lua.field (Lua.global "ngx", "INFO")
        in
            Lua.call0 log_ #[INFO_, Lua.fromString s]
        end
     
     fun notice (s: string): unit =
        let
            val log_ = Lua.field (Lua.global "ngx", "log")
            val NOTICE_ = Lua.field (Lua.global "ngx", "NOTICE")
        in
            Lua.call0 log_ #[NOTICE_, Lua.fromString s]
        end

    fun getRequestHeader (h: string): string option =
        let
            val get_headers_ = Lua.field (Lua.field (Lua.global "ngx", "req"), "get_headers")
            val req_headers = Lua.call1 get_headers_ #[]
        in
            SOME (Lua.unsafeFromValue (Lua.field (req_headers, h)) : string)
        end handle exc => NONE


    fun flushRequest (Request req) =
        let
            val Response resp = #response req : response
            val code = !(#code resp)
            val body = !(#body resp)
            val headers = !(#headers resp)
            
            val ngx_ = Lua.global "ngx"
            val header_ = Lua.field (ngx_, "header")
        in
            List.app (fn (k,v) => Lua.setField (header_, k, Lua.fromString v)) headers;
            Lua.setField (ngx_, "status", Lua.fromInt code);
            say body
        end

    fun setResponseBody (Request req) (body: string): unit  =
        let
            val Response resp = #response req : response
        in
            (#body resp) := body
        end
    
    fun setStatusCode (Request req) (code: int): unit  =
        let
            val Response resp = #response req : response
        in
            (#code resp) := code
        end
    
    fun setContentType (Request req) (ctype: string): unit  =
        let
            val Response resp = #response req : response
        in
            (#headers resp) := ("Content-Type", ctype)::(!(#headers resp))
        end

    fun setResponseHeader (Request req) (k: string) (v: string): unit  =
        let
            val Response resp = #response req : response
        in
            (#headers resp) := (k, v)::(!(#headers resp))
        end
    
    fun setStatusCode_ code =
        let
            val ngx_ = Lua.global "ngx"
        in
            Lua.setField (ngx_, "status", Lua.fromInt code)
        end

    fun send400 () =
        let
            val ngx_ = Lua.global "ngx"
        in
            Lua.setField (ngx_, "status", Lua.fromInt 400);
            say "<h1>400 - Bad Request</h1>"
        end
    
    fun send404 () =
        let
            val ngx_ = Lua.global "ngx"
        in
            Lua.setField (ngx_, "status", Lua.fromInt 404);
            say "<h1>404 - Not Found</h1>"
        end
    
    fun send500 () =
        let
            val ngx_ = Lua.global "ngx"
        in
            Lua.setField (ngx_, "status", Lua.fromInt 500);
            say "<h1>500 - Internal Error</h1>"
        end

    fun matchPattern (pparts: string list): (action_fn * (string * string) list) option =
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
            
            fun m [] pparts: (action_fn * ((string * string) list)) option = NONE
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
            m (!actions) pparts
        end

    and routeRequest (Request req): unit =
        let
            val path = (#path req)
        in
            let
                val pparts = String.fields (fn c => c = #"/") path
                val pparts = List.filter (fn p => p <> "") pparts
            in
                case matchPattern pparts of
                    NONE => send404 ()
                    | SOME (afn, path_args) => 
                        let in
                            (#path_args req) := path_args;
                            (#handler req) := SOME afn;
                            afn (Request req)
                        end handle
                            exc =>
                                let in
                                    notice ("Slick.routeRequest() exception in action fn("^path^"): "^(exnName exc));
                                    send500 ()
                                end
            end
        end

    fun extractPathQuery_ uri =
        let
            val uparts = String.fields (fn c => c = #"?") uri
            val uparts = List.filter (fn p => p <> "") uparts

            val (path, query) = case uparts of
                [p, q] => (p, SOME q)
                | [p] => (p, NONE)
                | _ => raise Fail ("Slick.extractPathQuery_() malformed uri: "^uri)
        in
            (path, query)
        end
    
    fun mkRequest (): request =
        let
            val uri = Lua.unsafeFromValue (Lua.field (Lua.field (Lua.global "ngx", "var"), "request_uri")) : string
            val host = Lua.unsafeFromValue (Lua.field (Lua.field (Lua.global "ngx", "var"), "host")) : string
            val scheme = Lua.unsafeFromValue (Lua.field (Lua.field (Lua.global "ngx", "var"), "scheme")) : string
            val (path, query) = extractPathQuery_ uri
            val req = Request {
                scheme  =   scheme,
                host    =   host,
                uri     =   uri,
                path    =   path,
                query   =   query,
                path_args = ref [],
                handler =   ref NONE,
                session =   ref NONE,
                mysql_db =  ref NONE,
                mailer  =   ref NONE,
                response =  Response {
                                code =   ref 200,
                                body =   ref "",
                                redirect = ref NONE,
                                headers = ref []
                            }
            }
        in
            req
        end   

    (*
     * SESSION
     *)
    exception CookieSessionInit of string
    fun sessionCookieInit (secret: string): unit =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.session"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val init = Lua.field (module, "init")
                    val t = Lua.newTable ()
                    val _ = Lua.setField (t, "remember", Lua.fromBool true)
                    val _ = Lua.setField (t, "audience", Lua.fromString "internet")
                    val _ = Lua.setField (t, "secret", Lua.fromString secret)
                    val _ = Lua.setField (t, "storage", Lua.fromString "cookie")

                in
                    Lua.call0 init #[t]
                end
            else
                raise CookieSessionInit "resty.session failed to initialize"
        end handle exc => raise CookieSessionInit "resty.session failed to initialize"

    
    exception CookieSessionStart of string
    fun sessionCookieStart (secret: string): Lua.value =
         let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.session"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val start = Lua.field (module, "start")
                    val t = Lua.newTable ()
                    val _ = Lua.setField (t, "secret", Lua.fromString secret)

                in
                    Lua.call1 start #[t]
                end
            else
                raise CookieSessionStart "resty.session failed to start"
        end handle exc => raise CookieSessionStart "resty.session failed to start"

    exception CookieSessionLogout of string
    fun sessionCookieLogout (): unit =
         let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.session"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val logout = Lua.field (module, "logout")

                in
                    Lua.call0 logout #[]
                end
            else
                raise CookieSessionLogout "resty.session failed to logout"
        end handle exc => raise CookieSessionLogout "resty.session failed to logout"

    fun sessionLogout (session: Lua.value): unit =
        Lua.method0 (session, "logout") #[]
    
    fun sessionSave (session: Lua.value): unit =
        Lua.method0 (session, "save") #[]

    fun sessionSetInt (session: Lua.value) (k: string) (v: int): unit =
        Lua.method0 (session, "set") #[Lua.fromString k, Lua.fromInt v]
    
    fun sessionSetReal (session: Lua.value) (k: string) (v: real): unit =
        Lua.method0 (session, "set") #[Lua.fromString k, Lua.fromReal v]
    
    fun sessionSetBool (session: Lua.value) (k: string) (v: bool): unit =
        Lua.method0 (session, "set") #[Lua.fromString k, Lua.fromBool v]
    
    fun sessionSetString (session: Lua.value) (k: string) (v: string): unit =
        Lua.method0 (session, "set") #[Lua.fromString k, Lua.fromString v]
    
    exception SessionGetException of string
    
    fun sessionGetInt (session: Lua.value) (k: string): int option =
        let
            val v = Lua.method1 (session, "get") #[Lua.fromString k]
        in
            if Lua.isNil v then
                NONE
            else
                SOME (Lua.unsafeFromValue v : int)
        end
    
    fun sessionGetReal (session: Lua.value) (k: string): real option =
        let
            val v = Lua.method1 (session, "get") #[Lua.fromString k]
        in
            if Lua.isNil v then
                NONE
            else
                SOME (Lua.unsafeFromValue v : real)
        end
    
    fun sessionGetBool (session: Lua.value) (k: string): bool option =
        let
            val v = Lua.method1 (session, "get") #[Lua.fromString k]
        in
            if Lua.isNil v then
                NONE
            else
                SOME (Lua.unsafeFromValue v : bool)
        end
    
    fun sessionGetString (session: Lua.value) (k: string): string option =
        let
            val v = Lua.method1 (session, "get") #[Lua.fromString k]
        in
            if Lua.isNil v then
                NONE
            else
                SOME (Lua.unsafeFromValue v : string)
        end
    
    (*
     * MYSQL
     *)
    val mysql_module_: Lua.value option ref = ref NONE
    val mysql_db_: Lua.value option ref = ref NONE
    
    fun mysqlRequireNew (cfg: Lua.value): Lua.value option =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.mysql"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val _ = mysql_module_ := SOME module
                    val (db, err) = Lua.method2 (module, "new") #[]
                in
                    if Lua.isFalsy db then
                        let
                            val err = Lua.unsafeFromValue err : string
                        in
                            setStatusCode_ 500;
                            say ("Slick.mysqlRequireNew() failed to instantiate mysql: "^err);
                            NONE
                        end
                    else
                        let
                            val (ok, err, errcode) = Lua.method3 (db, "connect") #[cfg]
                            val ok = Lua.unsafeFromValue ok : bool
                        in
                            if not ok then
                                let
                                    val err = Lua.unsafeFromValue err : string
                                    val errcode = Lua.unsafeFromValue errcode : string
                                in
                                    setStatusCode_ 500;
                                    say ("Slick.mysqlRequireNew() failed to connect to DB: "^err^": "^errcode);
                                    NONE
                                end
                            else
                                let in
                                    mysql_db_ := SOME db;
                                    SOME db
                                end
                        end
                end
            else
                let in
                    setStatusCode_ 500;
                    say "Slick.mysqlRequireNew() failed to instantiate mysql: pcall";
                    NONE
                end
        end

    fun extractRow_ NONE _ acc = acc
        | extractRow_ (SOME row) i acc =
            let
                val (next_i, next_v) = Lua.call2 Lua.Lib.next #[row, i]
            in
                if Lua.isNil next_v then
                    extractRow_ NONE next_i acc
                else
                    let
                        val next_i_str = Lua.unsafeFromValue next_i : string
                        val next_v_str = Lua.unsafeFromValue (Lua.call1 Lua.Lib.tostring #[next_v]) : string
                    in
                        extractRow_ (SOME row) next_i ((next_i_str, next_v_str)::acc)
                    end
            end

    fun extractRS_ NONE _ acc = acc
        | extractRS_ (SOME t) i acc =
            let
                val (next_i, next_v) = Lua.call2 Lua.Lib.next #[t, i]
            in
                if Lua.isNil next_v then
                    extractRS_ NONE next_i acc
                else
                    let
                        val next_i_int = Lua.unsafeFromValue next_i : int
                        val next_v_extracted = extractRow_ (SOME next_v) Lua.NIL []
                    in
                        extractRS_ (SOME t) next_i ((next_i_int, next_v_extracted)::acc)
                    end
            end

    fun rsToPairs_ (t: Lua.value): (int * (string * string) list) list =
        let in
            extractRS_ (SOME t) Lua.NIL []
        end

    fun mysqlQuery (db: Lua.value) (sql: string): (int * (string * string) list) list =
        let
            val res = Lua.method1 (db, "query") #[Lua.fromString sql]
        in
            List.rev (rsToPairs_ res)
        end

    exception InvalidColumn of string
    
    fun cInt (row: (string*string) list) (c: string): int option =
        let
            val res = case findPairValue c row of
                NONE => raise InvalidColumn ("Unknown column "^c)
                | SOME v => case v of
                    "userdata: NULL" => NONE
                    | _ => Int.fromString v
        in
            res
        end
    
    fun cReal (row: (string*string) list) (c: string): real option =
        let
            val res = case findPairValue c row of
                NONE => raise InvalidColumn ("Unknown column "^c)
                | SOME v => case v of
                    "userdata: NULL" => NONE
                    | _ => Real.fromString v
        in
            res
        end
    
    fun cBool (row: (string*string) list) (c: string): bool option =
        let
            val res = case findPairValue c row of
                NONE => raise InvalidColumn ("Unknown column "^c)
                | SOME v => case v of
                    "userdata: NULL" => NONE
                    | "0" => SOME false
                    | _ => SOME true
        in
            res
        end

    fun cString (row: (string*string) list) (c: string): string option =
        let
            val res = case findPairValue c row of
                NONE => raise InvalidColumn ("Unknown column "^c)
                | SOME v => case v of
                    "userdata: NULL" => NONE
                    | _ => SOME v
        in
            res
        end
 
    (*
     * GET args / POST args / UPLOAD files
     *)
    exception RequestArgs of string
    
    fun extractUploadFiles_ NONE _ acc = acc
        | extractUploadFiles_ (SOME files) i acc =
            let
                val (next_i, next_v) = Lua.call2 Lua.Lib.next #[files, i]
            in
                if Lua.isNil next_v then
                    extractUploadFiles_ NONE next_i acc
                else
                    let
                        val next_i_str = Lua.unsafeFromValue next_i : string
                        val next_file_t = extractRow_ (SOME next_v) Lua.NIL []
                    in
                        extractUploadFiles_ (SOME files) next_i ((next_i_str, next_file_t)::acc)
                    end
            end
    
    fun getRequestArgs (): (string * string) list * (string * string) list * (string * (string * string) list) list =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.reqargs"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val (get, post, files) = Lua.call3 module #[]
                    val get = extractRow_ (SOME get) Lua.NIL []
                    val post = extractRow_ (SOME post) Lua.NIL []
                    val files = extractUploadFiles_ (SOME files) Lua.NIL []
                in
                    
                    (get, post, files)
                end
            else
                raise RequestArgs "resty.reqargs failed to initialize"
        end

    (*
     * HTTP client
     *)
    fun httpcRequireNew (): Lua.value option =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.http"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val (httpc, err) = Lua.method2 (module, "new") #[]
                in
                    if Lua.isFalsy httpc then
                        let
                            val err = Lua.unsafeFromValue err : string
                        in
                            setStatusCode_ 500;
                            say ("Slick.httpcRequireNew() failed to instantiate HTTP client: "^err);
                            NONE
                        end
                    else
                        SOME httpc
                end
            else
                let in
                    setStatusCode_ 500;
                    say "Slick.httpcRequireNew() failed to instantiate HTTP client: pcall";
                    NONE
                end
        end

    fun httpGET (url: string) (headers: (string*string)list): (int*string*(string*string)list)option =
        let
            val httpc = httpcRequireNew ()
        in
            case httpc of
                NONE => (notice "Slick.httpGET() httpc is NONE. aborting."; NONE)
                | SOME httpc =>
                    let
                        val cfg = Lua.newTable ()
                        val _ = Lua.setField (cfg, "method", Lua.fromString "GET")
                        val _ = Lua.setField (cfg, "headers", List.foldl (fn ((k,v),acc) => (Lua.setField (acc, k, Lua.fromString v); acc))
                                                             (Lua.newTable ())
                                                             headers)
                        val _ = Lua.setField (cfg, "ssl_verify", Lua.fromBool false)
                        val (res, err) = Lua.method2 (httpc, "request_uri") #[Lua.fromString url, cfg]
                    in
                        if Lua.isFalsy res then
                            let
                                val err = Lua.unsafeFromValue err : string
                            in
                                notice ("Slick.httpGET() httpc:request_uri() error: "^err);
                                NONE
                            end
                        else
                            let
                                val code = Lua.unsafeFromValue (Lua.field (res, "status")) : int
                                val body = Lua.unsafeFromValue (Lua.field (res, "body")) : string
                                val hs = extractRow_ (SOME (Lua.field (res, "headers"))) Lua.NIL []
                            in
                                SOME (code, body, hs)
                            end
                    end
        end

    fun httpPOST (url: string) (raw_data: string) (headers: (string*string)list): (int*string*(string*string)list)option =
        let
            val httpc = httpcRequireNew ()
        in
            case httpc of
                NONE => (notice "Slick.httpPOST() httpc is NONE. aborting."; NONE)
                | SOME httpc =>
                    let
                        val cfg = Lua.newTable ()
                        val _ = Lua.setField (cfg, "method", Lua.fromString "POST")
                        val _ = Lua.setField (cfg, "body", Lua.fromString raw_data)
                        val _ = Lua.setField (cfg, "headers", List.foldl (fn ((k,v),acc) => (Lua.setField (acc, k, Lua.fromString v); acc))
                                                             (Lua.newTable ())
                                                             headers)
                        val _ = Lua.setField (cfg, "ssl_verify", Lua.fromBool false)
                        val (res, err) = Lua.method2 (httpc, "request_uri") #[Lua.fromString url, cfg]
                    in
                        if Lua.isFalsy res then
                            let
                                val err = Lua.unsafeFromValue err : string
                            in
                                notice ("Slick.httpPOST() httpc:request_uri() error: "^err);
                                NONE
                            end
                        else
                            let
                                val code = Lua.unsafeFromValue (Lua.field (res, "status")) : int
                                val body = Lua.unsafeFromValue (Lua.field (res, "body")) : string
                                val hs = extractRow_ (SOME (Lua.field (res, "headers"))) Lua.NIL []
                            in
                                SOME (code, body, hs)
                            end
                    end
        end

    (*
     * MAILER
     *)
    fun mailerRequireNew (cfg: Lua.value): Lua.value option =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.mail"]
            val ok = Lua.unsafeFromValue ok : bool
        in
            if ok then
                let
                    val new_ = Lua.field (module, "new")
                    val (mailer, err) = Lua.call2 new_ #[cfg]
                in
                    if Lua.isFalsy mailer then
                        let
                            val err = Lua.unsafeFromValue err : string
                        in
                            setStatusCode_ 500;
                            say ("Slick.mailRequireNew() failed to instantiate mailer: "^err);
                            NONE
                        end
                    else
                        SOME mailer
                end
            else
                let in
                    setStatusCode_ 500;
                    say "Slick.mailRequireNew() failed to instantiate mailer: pcall";
                    NONE
                end
        end
    
    fun sendHTMLMail (mailer: Lua.value) (from: string) (to: string list) (cc: string list) (subject: string) (body: string): bool =
        let
            val from = Lua.fromString from
            val to = stringListToLuaArray_ to
            val cc = stringListToLuaArray_ cc
            val subject = Lua.fromString subject
            val body = Lua.fromString body
            
            val cfg = Lua.newTable ()
            val _ = Lua.setField (cfg, "from", from)
            val _ = Lua.setField (cfg, "to", to)
            val _ = Lua.setField (cfg, "cc", cc)
            val _ = Lua.setField (cfg, "subject", subject)
            val _ = Lua.setField (cfg, "html", body)
            
            val (ok, err) = Lua.method2 (mailer, "send") #[cfg]
        in
            if Lua.isFalsy ok then
                let
                    val err = Lua.unsafeFromValue err : string
                in
                    notice ("Slick.sendHTMLMail() error sending mail: "^err);
                    false
                end
            else
                true  
        end

    (*
     * CRYPTO
     *)
    val md5_module_: Lua.value option ref = ref NONE
    val uuid_module_: Lua.value option ref = ref NONE
    fun initCrypto (): unit =
        let
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "md5"]
            val ok = Lua.unsafeFromValue ok : bool
            val _ = if ok then
                    md5_module_ := SOME module
                else
                    let in
                        setStatusCode_ 500;
                        say "Slick.initCrypto() failed to instantiate md5: pcall";
                    end
            
            val (ok, module) = Lua.call2 Lua.Lib.pcall #[Lua.Lib.require, Lua.fromString "resty.uuid"]
            val ok = Lua.unsafeFromValue ok : bool
            val _ = if ok then
                    uuid_module_ := SOME module
                else
                    let in
                        setStatusCode_ 500;
                        say "Slick.initCrypto() failed to instantiate resty.uuid: pcall";
                    end

        in
            ()
        end

    fun md5hex (s: string): string =
        let
            val md5hex_ = Lua.field (valOf(!md5_module_), "sumhexa")
        in
            Lua.unsafeFromValue (Lua.call1 md5hex_ #[Lua.fromString s]) : string
        end
    
    fun uuidv4 (): string =
        let
            val generate_ = Lua.field (valOf(!uuid_module_), "generate")
        in
            Lua.unsafeFromValue (Lua.call1 generate_ #[]) : string
        end

    val random_md_5_counter_ = ref 0
    fun randomMD5 (): string =
        let
            val _ = random_md_5_counter_ := !random_md_5_counter_ + 1
        in
            md5hex ((Int.toString (!random_md_5_counter_))^uuidv4 ()^(LargeInt.toString (Time.toMilliseconds (Time.now ()))))
        end
end

