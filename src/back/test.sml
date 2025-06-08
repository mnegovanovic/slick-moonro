structure TestActions =
struct

structure S = Slick
structure WA = WebAction
open SMUtils

do (
S.action ("/test/index", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>Slick&Monroo Tests</h1>"
            ^"<a href='/test/get_args'>GET args</a><br />"
            ^"<a href='/test/post_args'>POST args</a><br />"
            ^"<a href='/test/upload_files'>UPLOAD files</a><br />"
            ^"<a href='/test/session'>Session</a><br />"
            ^"<a href='/test/mysql_query'>MySQL query</a><br />"
            ^"<a href='/test/mysql_columns'>MySQL columns</a><br />"
            ^"<a href='/test/http_get'>HTTP client GET</a><br />"
            ^"<a href='/test/http_post'>HTTP client POST</a><br />"
            ^"<a href='/test/mailer'>Mailer</a><br />"
            ^"<a href='/test/crypto'>Crypto</a><br />"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/get_args", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>GET args</h1>"
            ^"<a href='/test/get_args_1?test1=aaa&test2=bbb'>GET args</a><br />"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/get_args_1", (fn (S.Request req) =>
    let
        val (get, _, _) = S.getRequestArgs ()
        val html = ref ("<html>"
            ^"<h1>GET args</h1>"
            ^"<p>")
    in
        case (findPairValue "test1" get) of
            NONE => html := (!html)^"test1: value is NONE<br />"
            | SOME v => html := (!html)^"test1: "^v^"<br />";
        
        case (findPairValue "test2" get) of
            NONE => html := (!html)^"test2: value is NONE<br />"
            | SOME v => html := (!html)^"test2: "^v^"<br />";
            
        html := (!html)^"</p>";
        html := (!html)^"</html>";

        S.setResponseBody (S.Request req) (!html);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/post_args", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>POST args</h1>"
            ^"<form action='/test/post_args_1' method='POST'>"
            ^"<input type='text' id='test1' name='test1' value='aaa'  />"
            ^"<input type='text' id='test2' name='test2' value='bbb'  />"
            ^"<button type='submit'>test</button>"
            ^"</form>"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/post_args_1", (fn (S.Request req) =>
    let
        val (_, post, _) = S.getRequestArgs ()
        val html = ref ("<html>"
            ^"<h1>POST args</h1>"
            ^"<p>")
    in
        case (findPairValue "test1" post) of
            NONE => html := (!html)^"test1: value is NONE<br />"
            | SOME v => html := (!html)^"test1: "^v^"<br />";
        
        case (findPairValue "test2" post) of
            NONE => html := (!html)^"test2: value is NONE<br />"
            | SOME v => html := (!html)^"test2: "^v^"<br />";
            
        html := (!html)^"</p>";
        html := (!html)^"</html>";

        S.setResponseBody (S.Request req) (!html);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/upload_files", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>UPLOAD files</h1>"
            ^"<form action='/test/upload_files_1' method='POST' enctype='multipart/form-data'>"
            ^"Select file to upload:<br />"
            ^"<input type='file' name='fileToUpload' id='fileToUpload' /><br />"
            ^"<button type='submit'>test upload</button>"
            ^"</form>"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/upload_files_1", (fn (S.Request req) =>
    let
        val (_, _, files) = S.getRequestArgs ()
        val html = ref ("<html>"
            ^"<h1>UPLOAD files</h1>"
            ^"<p>")
        val file = valOf (findPairValue "fileToUpload" files)
        fun echoFile_ [] acc = acc
            | echoFile_ ((k,v)::fs) acc = echoFile_ fs (acc^k^" -> "^v^"<br />")
    in
        html := (!html)^(echoFile_ file "");
        html := (!html)^"</p>";
        html := (!html)^"</html>";

        S.setResponseBody (S.Request req) (!html);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/session", (fn (S.Request req) =>
    let
        val session = valOf (!(#session req))

        val _ = S.sessionSetString session "test-string" "test-test"
        val _ = S.sessionSetBool session "test-bool" false
        val _ = S.sessionSetInt session "test-int" 100
        val _ = S.sessionSetReal session "test-real" 9.99
        val _ = S.sessionSave session
    in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>Session</h1>"
            ^"<p>Vars set, go <a href='/test/session_1'>here</a> to display</p>"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/session_1", (fn (S.Request req) =>
    let
        val session = valOf (!(#session req))
        val html = ref ("<html>"
            ^"<h1>Session var display</h1>"
            ^"<p>")
    in
        case (S.sessionGetString session "test-string") of
            NONE => html := (!html)^"test-string: value is NONE<br />"
            | SOME v => html := (!html)^"test-string: "^v^"<br />";
        
        case (S.sessionGetBool session "test-bool") of
            NONE => html := (!html)^"test-bool: value is NONE<br />"
            | SOME v => html := (!html)^"test-bool: "^(Bool.toString v)^"<br />";
        
        case (S.sessionGetInt session "test-int") of
            NONE => html := (!html)^"test-int: value is NONE<br />"
            | SOME v => html := (!html)^"test-int: "^(Int.toString v)^"<br />";
        
        case (S.sessionGetReal session "test-real") of
            NONE => html := (!html)^"test-real: value is NONE<br />"
            | SOME v => html := (!html)^"test-real: "^(Real.toString v)^"<br />";
            
        html := (!html)^"</p>";
        html := (!html)^"<p>Go <a href='/test/session_2'>here</a> to logout</p>";
        html := (!html)^"</html>";

        S.setResponseBody (S.Request req) (!html);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/session_2", (fn (S.Request req) =>
    let
        val _ = S.sessionCookieLogout ()
        val session = S.sessionCookieStart "xm6IiUyj9PLEZoad9Y7q"
        val html = ref ("<html>"
            ^"<h1>Session var display (we logged out)</h1>"
            ^"<p>")
    in
        case (S.sessionGetString session "test-string") of
            NONE => (html := (!html)^"test-string: value is NONE<br />"; S.notice "test-string: value is NONE")
            | SOME v => (html := (!html)^"test-string: "^v^"<br />"; S.notice ("test-string: "^v));
        
        case (S.sessionGetBool session "test-bool") of
            NONE => html := (!html)^"test-bool: value is NONE<br />"
            | SOME v => html := (!html)^"test-bool: "^(Bool.toString v)^"<br />";
        
        case (S.sessionGetInt session "test-int") of
            NONE => html := (!html)^"test-int: value is NONE<br />"
            | SOME v => html := (!html)^"test-int: "^(Int.toString v)^"<br />";
        
        case (S.sessionGetReal session "test-real") of
            NONE => html := (!html)^"test-real: value is NONE<br />"
            | SOME v => html := (!html)^"test-real: "^(Real.toString v)^"<br />";
        html := (!html)^"</p></html>";

        S.setResponseBody (S.Request req) (!html);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/mysql_query", (fn (S.Request req) =>
    let
        val db = valOf (!(#mysql_db req))
        val rs = S.myQuery db "select * from ads order by id asc;"
        
        fun loopRow_ [] acc = acc
            | loopRow_ ((c,v)::pairs) acc = loopRow_ pairs (acc^"<span style='color: red;'>"^c^"</span>->"^v^" ")
        fun loopRS_ [] acc = acc
            | loopRS_ ((i,row)::rows) acc =
                loopRS_ rows (acc^"<br /><span style='color: red;'>"^(Int.toString i)^"</span>"^(loopRow_ row ""))
    in
        S.setResponseBody (S.Request req) ("<html><p>Hello world from LunarML!</p>"^(loopRS_ rs "")^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/mysql_columns", (fn (S.Request req) =>
    let
        val db = valOf (!(#mysql_db req))
        val rs = S.myQuery db "select * from ads order by id asc limit 1;"
        val (_, row) = List.nth (rs, 0)
    in
        S.setResponseBody (S.Request req) ("<html>"
            ^"id: "^(case (S.cInt row "id") of SOME id => Int.toString id | NONE => "NULL")
            ^"<br />title: "^(case (S.cString row "title") of SOME title => title | NONE => "NULL")
            ^"<br />parttime: "^(case (S.cBool row "parttime") of SOME parttime => Bool.toString parttime | NONE => "NULL")
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/http_post", (fn (S.Request req) =>
    case S.httpPOST "https://httpbin.org/post?a=a&b=b"
                    (httpEncodePOSTArgs [("test1", "test1"), ("test2", "test2")])
                    [("Test-Header", "test")] of
        SOME (code, body, hs) =>
            let in
                S.setResponseBody (S.Request req) ((Int.toString code)
                                                  ^"<br />"^body
                                                  ^"<br />"^(List.foldl (fn ((k,v), acc) => acc^"<br />"^k^"->"^v) "" hs));
                S.setContentType (S.Request req) "text/html";
                S.flushRequest (S.Request req)
            end
        | NONE =>
            let in
                S.setResponseBody (S.Request req) "httpc returned NONE";
                S.setContentType (S.Request req) "text/html";
                S.flushRequest (S.Request req)
            end
));

S.action ("/test/http_get", (fn (S.Request req) =>
    case S.httpGET "https://httpbin.org/get?a=a&b=b"
                    [("Test-Header", "test")] of
        SOME (code, body, hs) =>
            let in
                S.setResponseBody (S.Request req) ((Int.toString code)
                                                  ^"<br />"^body
                                                  ^"<br />"^(List.foldl (fn ((k,v), acc) => acc^"<br />"^k^"->"^v) "" hs));
                S.setContentType (S.Request req) "text/html";
                S.flushRequest (S.Request req)
            end
        | NONE =>
            let in
                S.setResponseBody (S.Request req) "httpc returned NONE";
                S.setContentType (S.Request req) "text/html";
                S.flushRequest (S.Request req)
            end
));

S.action ("/test/mailer", (fn (S.Request req) =>
    let
        val mailer = valOf (!(#mailer req))
        val res = (S.sendHTMLMail mailer
            "kiao.desouzza@gmail.com" (* from *)
            ["milos.negovanovic@hotmail.com"] (* to *)
            ["milosn@yahoo.com"] (* cc *)
            "TEST SUBJECT" (* subject *)
            "<html><h1>test mail</h1></html>") (* body *)
    in
        S.setResponseBody (S.Request req) (if res then "OK" else "ERR");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/test/crypto", (fn (S.Request req) =>
    let
        val test_hex = S.md5hex "test"
        val test_test_hex = S.md5hex "test test"
        val uuid1 = S.uuidv4 ()
        val uuid2 = S.uuidv4 ()
        val rand_md5_1 = S.randomMD5 ()
        val rand_md5_2 = S.randomMD5 ()
    in
        S.setResponseBody (S.Request req) ("<html>"
            ^"<h1>CRYPTO</h1>"
            ^"test -> "^test_hex^(if test_hex = "098f6bcd4621d373cade4e832627b4f6" then " OK" else " FAIL")^"<br />"
            ^"test test -> "^test_test_hex^(if test_test_hex = "4f4acc5d8c71f5fbf04dace00b5360c8" then " OK" else " FAIL")^"<br />"
            ^"uuid1 -> "^uuid1^"<br />"
            ^"uuid2 -> "^uuid2^"<br />"
            ^"randomMD5 -> "^rand_md5_1^"<br />"
            ^"randomMD5 -> "^rand_md5_2^"<br />"
            ^"</html>");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
))

)end
