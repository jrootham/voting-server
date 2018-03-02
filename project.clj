(defproject voting "0.1.0-SNAPSHOT"
  :description "Cabal voting back end"
  :url "http://example.com/FIXME"
  :dependencies 
    [
				[org.clojure/clojure "1.8.0"]
				[ring/ring-core "1.6.2"]
				[ring/ring-jetty-adapter "1.6.2"]
				[com.unbounce/encors "2.3.0"]
				[ring/ring-json "0.4.0"]
				[org.clojure/clojure "1.8.0"]
				[org.postgresql/postgresql "9.4-1206-jdbc41"]
				[org.clojure/java.jdbc "0.7.1"]
				[org.clojure/data.json "0.2.6"]
				[compojure "1.6.0"]
				[bananaoomarang/ring-debug-logging "1.1.0"]
     ]
  :min-lein-version "2.0.0"
  :main ^:skip-aot voting_server.core
  :aot [voting_server.core]
  :target-path "target/%s"
  :uberjar-name "voting-server.jar"
)
