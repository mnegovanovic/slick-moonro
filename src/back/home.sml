structure HomeActions =
struct

structure S = Slick
structure WA = WebAction
open SMUtils

do (
S.action ("/", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) (S.readFile "./static/app.html");
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/register_enquiry", (fn (S.Request req) =>
    let
        val errs = ref []
        val (_, post, _) = S.getRequestArgs ()
        
        val name = strip (valOf (S.findPairValue_ "name" post)) [#" ", #"\t"]
        val _ = if (String.size name) < 1 then errs := ("name", "Missing name")::(!errs) else ()
        val email = strip (valOf (S.findPairValue_ "email" post)) [#" ", #"\t"]
        val _ = if (String.size email) < 1 then errs := ("email", "Missing email")::(!errs) else ()
        val msg = strip (valOf (S.findPairValue_ "msg" post)) [#" ", #"\t"]
        val _ = if (String.size msg) < 1 then errs := ("msg", "Missing message")::(!errs) else ()
        
        val res = if (List.length (!errs)) > 0 then
                WA.Result {
                    ok=false,
                    msg="Something is wrong, please have a look",
                    errs=(!errs)}
            else
                let
                    val mailer = valOf (!(#mailer req))
                    val mail_res = (S.sendHTMLMail mailer
                        "kiao.desouzza@gmail.com" (* from *)
                        ["milosn@atreides-host.net"] (* to *)
                        [email] (* cc *)
                        ("Atreides Host Web Enquiry - "^name) (* subject *)
                        ("<html><h1>"^name^" - "^email^"</h1><p>"^msg^"</p></html>"))
                in
                    if mail_res then
                        WA.Result {
                            ok=true,
                            msg="Your message has been sent",
                            errs=[]}
                    else
                        WA.Result {
                            ok=false,
                            msg="Error sending mail, please check your details and try again",
                            errs=[]}
                end
    in
        S.setResponseBody (S.Request req) (WA.dump res);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/hello_world", (fn (S.Request req) =>
    let in
        S.setResponseBody (S.Request req) "Hello world from Slick&Moonro!";
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));

S.action ("/md/[page]", (fn (S.Request req) =>
    let
        val page = valOf (S.findPairValue_ "page" (!(#path_args req)))
        val md = case page of
            "moonro-index-1" => S.readFile "./static/misc/moonro-md/index1.md"
            | "moonro-index-2" => S.readFile "./static/misc/moonro-md/index2.md"
            | "a-host-1" => S.readFile "./static/misc/a-host-md/1.md"
            | "a-host-2" => S.readFile "./static/misc/a-host-md/2.md"
            | "a-host-3" => S.readFile "./static/misc/a-host-md/3.md"
            | _ => S.readFile "./static/misc/moonro-md/404.md"
    in
        S.setResponseBody (S.Request req) md;
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
));


S.action ("/sm/fe_session_get", (fn (S.Request req) =>
    let
        val session = [("user", "anon")]
    in
        S.setResponseBody (S.Request req) (FESession.dump session);
        S.setContentType (S.Request req) "text/html";
        S.flushRequest (S.Request req)
    end
))

)end

