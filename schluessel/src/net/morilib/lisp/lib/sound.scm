;;;
;;; Copyright 2009-2011 Yuichiro Moriguchi
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define-java-subr
  make-audio-format
  net.morilib.lisp.sound.LispAudioFormat$MakeAudioFormat)
(define-java-subr
  open-data-line
  net.morilib.lisp.sound.LispDataLineSubrs$OpenDataLine)
(define-java-subr
  start-data-line
  net.morilib.lisp.sound.LispDataLineSubrs$StartDataLine)
(define-java-subr
  stop-data-line
  net.morilib.lisp.sound.LispDataLineSubrs$StopDataLine)
(define-java-subr
  flush-data-line
  net.morilib.lisp.sound.LispDataLineSubrs$FlushDataLine)
(define-java-subr
  close-data-line
  net.morilib.lisp.sound.LispDataLineSubrs$CloseDataLine)
(define-java-subr
  make-source-data-line
  net.morilib.lisp.sound.LispSourceDataLine$MakeSourceDataLine)
(define-java-subr
  write-data-line-relative
  net.morilib.lisp.sound.LispSourceDataLine$WriteDataLineRelative)
(define-java-subr
  make-target-data-line
  net.morilib.lisp.sound.LispTargetDataLine$MakeTargetDataLine)
(define-java-subr
  read-data-line-relative
  net.morilib.lisp.sound.LispTargetDataLine$ReadDataLineRelative)

(define-java-subr
  make-playable
  net.morilib.lisp.sound.Playable$MakePlayable)
(define-java-subr
  play-sound
  net.morilib.lisp.sound.Playable$PlaySound)
(define-java-subr
  stop-play-sound
  net.morilib.lisp.sound.Playable$StopPlaySound)
(define-java-subr
  pause-play-sound
  net.morilib.lisp.sound.Playable$PausePlaySound)

;; END
