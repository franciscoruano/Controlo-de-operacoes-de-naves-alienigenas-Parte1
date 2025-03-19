module TrabalhoFinal where

------------------------------------------------------------------------------------------------------------------
               -- Função principal
main :: IO ()
main = do
    putStrLn "\n TRABALHO PRÁTICO PARTE 1 - PROGRAMAÇÃO FUNCIONAL \n"
    putStrLn "Trabalho realizado por : \n Francisco Ruano Al 78474\n"

    -- Localização Inicial
    putStrLn "Qual é a localização Inicial que pretendes? (1 2 3)"
    loc <- getLine
    let [x, y, z] = map read (words loc) :: [Int]
    let coord = (x, y, z) -- Representa as coordenadas

    -- Se está ligada ou desligada a nave inicialmente
    putStrLn "Qual é o Estado (Ligado ou Desligado)? (True ou False)"
    lig <- getLine
    let ligado = read lig :: Bool
    let estadoInicial = (coord, ligado)

    putStrLn ("O estado da aeronave inicialmente é: " ++ show coord ++ ", " ++ show ligado)
    menu estadoInicial

------------------------------------------------------------------------------------------------------------------

type Localizacao = (Int, Int, Int)
type EstadoNave = (Localizacao, Bool)
type Movimentacao = (Int, Int, Int)
type ID = Int
type ListaNaves = ([Movimentacao], ID)

------------------------------------------------------------------------------------------------------------------
                -- Funções Auxiliares
atualiza_acao' :: Bool -> EstadoNave -> EstadoNave
atualiza_acao' ligacao (localizacao, _) = (localizacao, ligacao)

move' :: Movimentacao -> EstadoNave -> EstadoNave
move' (dx, dy, dz) ((x, y, z), ligacao) = ((x + dx, y + dy, z + dz), ligacao)

move_lista' :: [Movimentacao] -> EstadoNave -> EstadoNave
move_lista' [] estado = estado
move_lista' (mov:movs) estado = 
    let novoEstado = move' mov estado
    in move_lista' movs novoEstado

move_varios' :: [ListaNaves] -> [EstadoNave] -> [(EstadoNave, ID)]
move_varios' [] _ = []
move_varios' _ [] = error "Número insuficiente de estados iniciais para as naves."
move_varios' ((movs, id):restoNaves) (estado:restoEstados) = 
    let estadoFinal = move_lista' movs estado
    in (estadoFinal, id) : move_varios' restoNaves restoEstados

-- Função para verificar embates entre as naves
verifica_embates' :: EstadoNave -> [(EstadoNave, ID)] -> Bool
verifica_embates' (local, _) estados = 
    length [id | ((loc, _), id) <- estados, loc == local] > 1
    
------------------------------------------------------------------------------------------------------------------
-- Função para atualizar o estado da nave (ligar/desligar)
atualiza_acao :: EstadoNave -> IO EstadoNave
atualiza_acao estadoAtual = do
    putStrLn "Qual é o Estado (Ligado ou Desligado)? (True ou False)"
    lig <- getLine
    let ligado = read lig :: Bool
    let estado_Up = atualiza_acao' ligado estadoAtual
    putStrLn $ "O novo estado da nave é: " ++ show estado_Up
    return estado_Up

------------------------------------------------------------------------------------------------------------------
-- Função para receber a movimentação e atualizar o estado da nave
move :: EstadoNave -> IO EstadoNave
move estadoAtual@(coord, ligado) = do
    if not ligado
        then do
            putStrLn "A nave Encontra-se Desligada. Deseja ligá-la para se mover? (True, False)"
            ligar <- getLine
            let ligarNave = read ligar :: Bool
            if ligarNave
                then do
                    let estadoLigado = (coord, True)
                    putStrLn "A nave foi ligada."
                    move estadoLigado
                else do
                    putStrLn "A nave permanece desligada, logo nao se pode movimentar."
                    return estadoAtual
        else do
            putStrLn "Digite a movimentação que deseja ((1, 2, 3)):"
            mov <- getLine
            let moviment = read mov :: Movimentacao
            let estado_move = move' moviment estadoAtual
            putStrLn $ "O novo estado da nave é: " ++ show estado_move
            return estado_move

------------------------------------------------------------------------------------------------------------------
-- Função para receber múltiplas movimentações
move_lista :: EstadoNave -> IO EstadoNave
move_lista estadoAtual@(coord, ligado) = do
    if not ligado
        then do
            putStrLn "A nave Encontra-se Desligada. Deseja ligá-la para se mover? (True, False)"
            ligar <- getLine
            let ligarNave = read ligar :: Bool
            if ligarNave
                then do
                    let estadoLigado = (coord, True)
                    putStrLn "A nave foi ligada."
                    move_lista estadoLigado
                else do
                    putStrLn "A nave permanece desligada, logo nao se pode movimentar."
                    return estadoAtual
        else do
            putStrLn "Digite uma lista de movimentos (Exemplo: (1,2,3) (3,2,1) ...):"
            mov <- getLine
            let movs = map read (words mov) :: [Movimentacao]
            let estadoNovo = move_lista' movs estadoAtual
            putStrLn $ "O novo estado da nave é: " ++ show estadoNovo
            return estadoNovo

------------------------------------------------------------------------------------------------------------------
-- Função para mover várias naves e retornar os estados finais
move_varios :: IO [(EstadoNave, ID)]
move_varios = do
    putStrLn "Digite a lista de naves (movimentos,ID) ( [([(1,2,3), (4,5,6)], 12345), ([(1,1,1), (2,2,2)], 67890)] ):"
    naves <- getLine
    let navesLista = read naves :: [ListaNaves]

    putStrLn "Digite os estados iniciais das naves ( [((1,2,3),True), ((4,5,6),False), ((1,1,1),True), ((2,2,2),False)] ):"
    ests <- getLine
    let estadosIniciais = read ests :: [EstadoNave]

    let resultados = move_varios' navesLista estadosIniciais
    putStrLn $ "\nOs estados finais das naves são: " ++ show resultados
    return resultados

------------------------------------------------------------------------------------------------------------------
-- Função para verificar embates após a movimentação
verifica_embates :: [(EstadoNave, ID)] -> IO ()
verifica_embates resultados = do
    let embates = [(estado, id) | (estado, id) <- resultados, verifica_embates' estado resultados]
    if null embates
        then putStrLn "Não houve embates."
        else putStrLn $ "Houve embates nas seguintes naves: " ++ show embates

------------------------------------------------------------------------------------------------------------------
-- Função para mover várias naves e retornar os estados finais sem embates
move_varios_atualizada :: IO [(EstadoNave, ID)]
move_varios_atualizada = do
    putStrLn "Digite a lista de naves (movimentos,ID) ( [([(1,0,0), (0,1,0)], 1), ([(1,0,0), (0,0,1)], 2)] ):"  
    naves <- getLine
    let navesLista = read naves :: [ListaNaves]

    putStrLn "Digite os estados iniciais das naves ([((1,2,3), True), ((1,2,3), True)] ):"  
    ests <- getLine
    let estadosIniciais = read ests :: [EstadoNave]

    let resultados = move_varios' navesLista estadosIniciais
    let estadosFinais = filter (\(estado, id) -> not (verifica_embates' estado resultados)) resultados
    putStrLn $ "\nOs estados finais das naves sem embates são: " ++ show estadosFinais
    return estadosFinais

------------------------------------------------------------------------------------------------------------------
-- Menu principal atualizado
menu :: EstadoNave -> IO ()
menu estadoAtual = do 
    putStrLn "\nSelecione a opção que deseja visualizar: \n1- Atualiza Ação \n2- Move \n3- Move_Lista \n4- Move_Varios \n5- Verificar_Embates \n6- Move_Varios_Atualizada"
    putStrLn "Opção:"
    op <- getLine

    case op of
        "1" -> do
            estado_Up <- atualiza_acao estadoAtual
            menu estado_Up

        "2" -> do
            estado_move <- move estadoAtual
            menu estado_move

        "3" -> do
            estado_move <- move_lista estadoAtual
            menu estado_move

        "4" -> do
            resultados <- move_varios
            menu estadoAtual

        "5" -> do
            resultados <- move_varios
            verifica_embates resultados
            menu estadoAtual

        "6" -> do
            resultados <- move_varios_atualizada
            menu estadoAtual

        _ -> putStrLn "Opção Inválida. Tente novamente."

