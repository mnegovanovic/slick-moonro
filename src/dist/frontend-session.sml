structure FESession = struct
    open SMUtils
    
    type fe_session = (string * string) list

    fun dump (session: fe_session): string =
        let
            val repr = List.foldl (fn ((k,v), acc) => acc^(urlencode k)^" "^(urlencode v)^"\n") "" session
        in
            repr
        end

    fun load (repr: string): fe_session =
        let
            val lines = String.fields (fn c => c = #"\n") repr
            val lines = List.filter (fn p => p <> "") lines
            
            datatype state = SESSION_VAL
            val state = ref SESSION_VAL

            val session: (string * string) list ref = ref []
            
            fun process_ [] = ()
                | process_ (l::lines) =
                    case !state of
                        SESSION_VAL =>
                            let
                                val parts = String.fields (fn c => c = #" ") l
                                val k = urldecode (List.nth (parts, 0))
                                val v = urldecode (List.nth (parts, 1))
                            in
                                session := (k,v)::(!session);
                                process_ lines
                            end
        in
            process_ lines;
            !session
        end
end

