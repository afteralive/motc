#lang racket/base

;;;
;;; motc (mot Compiler)
;;; (C)opyright 2023 Shan Ran
;;;

(require
    "parser.rkt"
    "writer.rkt")

;;; basic forms:
;;; X=Y;
;;; X[I]=Y;
;;; with(Process[I]) {...}

(define ?sample-code "data_motLong = 10;
data_motStep[0] = 0;
data_motPosX[0] = 36;
data_motPosY[0] = 28;
data_motStep[1] = 10;
data_motPosX[1] = 36;
data_motPosY[1] = 28;
with(Process[0]){
data_motPartAngle[0] = 4;
data_motPartAngle[1] = 4;
}
with(Process[1]){
}
with(Process[2]){
data_motPartPosDir[0] = 165.57;
data_motPartPosDis[0] = 70.22;
data_motPartPosDir[1] = 165.57;
data_motPartPosDis[1] = 70.22;
}")

;;; TODO: Command-line interface