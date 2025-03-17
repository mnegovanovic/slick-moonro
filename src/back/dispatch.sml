structure export =
struct
    structure S = Slick
    
    fun dispatch () =
        let
            val _ = S.initCrypto ()
            val _ = S.sessionCookieInit "test"
            
            fun connectMySql (): Lua.value option =
                let 
                    val cfg = Lua.newTable ()
                in
                    Lua.setField (cfg, "host", Lua.fromString (S.getENVString "MYSQL_HOST"));
                    Lua.setField (cfg, "port", Lua.fromInt (S.getENVInt "MYSQL_PORT"));
                    Lua.setField (cfg, "database", Lua.fromString (S.getENVString "MYSQL_DB"));
                    Lua.setField (cfg, "user", Lua.fromString (S.getENVString "MYSQL_USER"));
                    Lua.setField (cfg, "password", Lua.fromString (S.getENVString "MYSQL_PASS"));
                    Lua.setField (cfg, "charset", Lua.fromString "utf8");
                    Lua.setField (cfg, "max_packet_size", Lua.fromInt (1024 * 1024));
                    
                    S.mysqlRequireNew cfg
                end
            
            fun setupMailer (): Lua.value option =
                let 
                    val cfg = Lua.newTable ()
                in
                    Lua.setField (cfg, "host", Lua.fromString (S.getENVString "MAILER_HOST"));
                    Lua.setField (cfg, "port", Lua.fromInt (S.getENVInt "MAILER_PORT"));
                    Lua.setField (cfg, "starttls", Lua.fromBool true);
                    Lua.setField (cfg, "username", Lua.fromString (S.getENVString "MAILER_USER"));
                    Lua.setField (cfg, "password", Lua.fromString (S.getENVString "MAILER_PASS"));
                    
                    S.mailerRequireNew cfg
                end

            val (S.Request req) = S.mkRequest ()
        in
            (*(#mysql_db req) := connectMySql ();
            (#mailer req) := setupMailer ();*)
            (#session req) := SOME (S.sessionCookieStart "test");
            
            S.routeRequest (S.Request req)
        end handle exc =>
            let in
                S.notice ("export.dispatch() exception "^(exnName exc));
                S.send500 ()
            end
end

