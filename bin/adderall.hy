#! /usr/bin/env hy
;; adderall - miniKanren in Hy
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see <http://www.gnu.org/licenses/>.

(import [sys]
        [argparse]
        [hy]
        [hy.cmdline [HyREPL]]
        [hy.completer [completion]])

(defmacro with/-> [topic &rest body]
  (setv new-body (map (fn [node]
                        `(~(first node) ~topic ~@(rest node)))
                      body))
  `(do ~@new-body))

(defn launch-repl []
  (setv sys.ps1 ";=> ")
  (setv sys.ps2 "    ")

  (with [(completion)]
        (setv hr (HyREPL))
        (with/-> hr
                 (.runsource "(import [adderall.dsl [*]])")
                 (.runsource "(require adderall.dsl)")
                 (.runsource "(require adderall.debug)")
                 (.interact "adderall"))))

(def parser (apply argparse.ArgumentParser []
                   {"prog" "adderall"
                    "usage" "%(prog)s [options]"
                    "formatter_class" argparse.RawDescriptionHelpFormatter}))

(apply parser.add_argument ["args"]
       {"nargs" argparse.REMAINDER
        "help" argparse.SUPPRESS})

(def options (.parse_args parser (rest sys.argv)))

(launch-repl)
