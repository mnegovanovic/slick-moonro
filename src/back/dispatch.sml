structure S = Slick

structure export = struct
    fun dispatch () =
        let
            val _ = S.initCrypto ()
            val _ = S.sessionCookieInit "test"
            
            fun connectMySql (): Lua.value option =
                let 
                    val cfg = Lua.newTable ()
                in
                    Lua.setField (cfg, "host", Lua.fromString "127.0.0.1");
                    Lua.setField (cfg, "port", Lua.fromInt 3306);
                    Lua.setField (cfg, "database", Lua.fromString "test");
                    Lua.setField (cfg, "user", Lua.fromString "test");
                    Lua.setField (cfg, "password", Lua.fromString "test");
                    Lua.setField (cfg, "charset", Lua.fromString "utf8");
                    Lua.setField (cfg, "max_packet_size", Lua.fromInt (1024 * 1024));
                    
                    S.mysqlRequireNew cfg
                end
            
            fun setupMailer (): Lua.value option =
                let 
                    val cfg = Lua.newTable ()
                in
                    Lua.setField (cfg, "host", Lua.fromString "smtp.gmail.com");
                    Lua.setField (cfg, "port", Lua.fromInt 587);
                    Lua.setField (cfg, "starttls", Lua.fromBool true);
                    Lua.setField (cfg, "username", Lua.fromString "kiao.desouzza@gmail.com");
                    Lua.setField (cfg, "password", Lua.fromString "");
                    
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

