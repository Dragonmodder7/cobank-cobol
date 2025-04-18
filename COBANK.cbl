IDENTIFICATION DIVISION.
PROGRAM-ID. COBANK.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

01 MAX-CONTAS         PIC 9(2) VALUE 10.
01 CONTADOR-CONTAS    PIC 9(2) VALUE 0.
01 OPCAO              PIC 9.
01 INDICE             PIC 9(2).

01 CONTA.
   05 NUMERO-CONTA     PIC 9(5).
   05 NOME-TITULAR     PIC A(30).
   05 SALDO-CONTA      PIC 9(6)V99.

01 LISTA-CONTAS.
   05 CONTAS OCCURS 10 TIMES.
      10 L-NUMERO      PIC 9(5).
      10 L-NOME        PIC A(30).
      10 L-SALDO       PIC 9(6)V99.

01 VALOR-OPERACAO     PIC 9(6)V99.
01 BUSCA-CONTA        PIC 9(5).
01 ENCONTRADA         PIC X VALUE "N".
01 POSICAO            PIC 9(2).

PROCEDURE DIVISION.

INICIO.
    PERFORM MENU UNTIL OPCAO = 6
    DISPLAY "Saindo do COBANK..."
    STOP RUN.

MENU.
    DISPLAY "============================="
    DISPLAY "       BEM-VINDO AO COBANK"
    DISPLAY "1 - Criar Conta"
    DISPLAY "2 - Consultar Saldo"
    DISPLAY "3 - Depositar"
    DISPLAY "4 - Sacar"
    DISPLAY "5 - Listar Contas"
    DISPLAY "6 - Sair"
    DISPLAY "Escolha uma opção: "
    ACCEPT OPCAO

    EVALUATE OPCAO
        WHEN 1
            PERFORM CRIAR-CONTA
        WHEN 2
            PERFORM CONSULTAR-SALDO
        WHEN 3
            PERFORM DEPOSITAR
        WHEN 4
            PERFORM SACAR
        WHEN 5
            PERFORM LISTAR-CONTAS
        WHEN 6
            CONTINUE
        WHEN OTHER
            DISPLAY "Opção inválida."
    END-EVALUATE.

CRIAR-CONTA.
    IF CONTADOR-CONTAS >= MAX-CONTAS THEN
        DISPLAY "Limite de contas atingido."
        EXIT PARAGRAPH
    END-IF

    ADD 1 TO CONTADOR-CONTAS
    DISPLAY "Número da nova conta: "
    ACCEPT L-NUMERO(CONTADOR-CONTAS)
    DISPLAY "Nome do titular: "
    ACCEPT L-NOME(CONTADOR-CONTAS)
    MOVE 0 TO L-SALDO(CONTADOR-CONTAS)
    DISPLAY "Conta criada com sucesso!".

CONSULTAR-SALDO.
    DISPLAY "Digite o número da conta: "
    ACCEPT BUSCA-CONTA
    MOVE "N" TO ENCONTRADA

    PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR-CONTAS
        IF L-NUMERO(INDICE) = BUSCA-CONTA THEN
            MOVE "S" TO ENCONTRADA
            MOVE INDICE TO POSICAO
        END-IF
    END-PERFORM

    IF ENCONTRADA = "S" THEN
        DISPLAY "Titular: " L-NOME(POSICAO)
        DISPLAY "Saldo: R$" L-SALDO(POSICAO)
    ELSE
        DISPLAY "Conta não encontrada."
    END-IF.

DEPOSITAR.
    DISPLAY "Número da conta: "
    ACCEPT BUSCA-CONTA
    MOVE "N" TO ENCONTRADA

    PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR-CONTAS
        IF L-NUMERO(INDICE) = BUSCA-CONTA THEN
            MOVE "S" TO ENCONTRADA
            MOVE INDICE TO POSICAO
        END-IF
    END-PERFORM

    IF ENCONTRADA = "S" THEN
        DISPLAY "Valor para depositar: "
        ACCEPT VALOR-OPERACAO
        ADD VALOR-OPERACAO TO L-SALDO(POSICAO)
        DISPLAY "Depósito realizado com sucesso."
    ELSE
        DISPLAY "Conta não encontrada."
    END-IF.

SACAR.
    DISPLAY "Número da conta: "
    ACCEPT BUSCA-CONTA
    MOVE "N" TO ENCONTRADA

    PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR-CONTAS
        IF L-NUMERO(INDICE) = BUSCA-CONTA THEN
            MOVE "S" TO ENCONTRADA
            MOVE INDICE TO POSICAO
        END-IF
    END-PERFORM

    IF ENCONTRADA = "S" THEN
        DISPLAY "Valor para saque: "
        ACCEPT VALOR-OPERACAO
        IF VALOR-OPERACAO > L-SALDO(POSICAO) THEN
            DISPLAY "Saldo insuficiente."
        ELSE
            SUBTRACT VALOR-OPERACAO FROM L-SALDO(POSICAO)
            DISPLAY "Saque realizado com sucesso."
        END-IF
    ELSE
        DISPLAY "Conta não encontrada."
    END-IF.

LISTAR-CONTAS.
    IF CONTADOR-CONTAS = 0 THEN
        DISPLAY "Nenhuma conta cadastrada."
        EXIT PARAGRAPH
    END-IF

    DISPLAY "===== LISTA DE CONTAS ====="
    PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR-CONTAS
        DISPLAY "Conta: " L-NUMERO(INDICE)
        DISPLAY "Titular: " L-NOME(INDICE)
        DISPLAY "Saldo: R$" L-SALDO(INDICE)
        DISPLAY "-----------------------------"
    END-PERFORM.
