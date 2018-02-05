menu :: IO String
menu = do { putStrLn menutxt
; putStr "Opcao: "
; c <- getLine
; return c
}
    where menutxt = unlines ["",
                                "Apostar ........... 1",
                                "Gerar nova chave .. 2",
                                "",
                                "Sair .............. 0"]