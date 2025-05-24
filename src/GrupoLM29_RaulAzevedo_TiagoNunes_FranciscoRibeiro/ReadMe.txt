O trabalho foi realizado por:
Raul Azevedo - 111479
Tiago Nunes - 123298
Francisco Ribeiro - 123930

Neste trabalho já foram implementadas as seguintes funcionalidades:
T1 - randomMove
T2 - play
T3 - playRandomly
T4 - representar o tabuleiro em forma de lista com '.' para Empty, e 'B' ou 'W' para os jogadores

Como no enunciado nao especificava quem jogava primeiro criou-se uma função que determina quem joga com base no número de
elementos da lista de coordenadas livres (uma espécie de se for par joga um, se for ímpar joga outro).

No main dá para testar as funções, não perdi muito tempo com isso, simplesmente pedi à AI para me criar testes só para ver se estava tudo a funcionar.
Só faltava impedir que o jogador jogasse numa casa já ocupada, mas já foi corrigido.

No entanto é um main simples pelo que todas as jogadas sao implementadas com o tabuleiro original, ou seja tem sempre todas as casas livres.
Mas foi suficiente para ver que as funções estavam a funcionar.

O nosso MyRandom possui um nexCoord.

No T3, o playRandomly, retorna Board e o Play retorna Option[Board], o que significa que o playRandomly devolve sempre um tabuleiro, enquanto o play devolve um tabuleiro ou None.
Nao sei se faz muito sentido porque se nao houver jogadas possiveis, podia retornar None também.
De qualquer maneira eu reutilizei o play que já fazia as validacoes necessarias para jogar e a alteracao na lista de coordenadas livres.