*&---------------------------------------------------------------------*
*& Report  ZWM_PALLET_MOV_JOB
*&
*&---------------------------------------------------------------------*
*& Criado por: ROFF SDF, Joao Abrantes 09.05.2013
*& Objectivo: Executar o movimento de mercadoria, substituindo
*&            o antigo processo existente na MIGO, por um JOB
*&---------------------------------------------------------------------*

REPORT  zwm_pallet_mov_job.

INCLUDE zwm_pallet_mov_job_top.
INCLUDE zwm_pallet_mov_job_f01.

START-OF-SELECTION.

* Seleciona dados tabela hardcoded.

  PERFORM get_parametrizacao.

* Seleciona os dados relevantes para processamento
  PERFORM seleciona_dados CHANGING gv_nodata.
* Se não existem dados na tabela Z, não necessitamos continuar
* o processo.
  CHECK gv_nodata IS INITIAL.

END-OF-SELECTION.
* Executa os respectivos movimentos de mercadoria.
  PERFORM executa_movimentos.
