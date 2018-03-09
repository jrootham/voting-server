(ns voting_server.core
  (:gen-class)
	(:require [com.unbounce.encors :refer [wrap-cors]])
	(:use ring.adapter.jetty)
	(:require [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
         [ring.util.response :refer [response]])
	(:require [ring.middleware.session :refer[wrap-session]])
	(:require [compojure.core :refer :all])
	(:require [clojure.java.jdbc :refer :all :as jdbc])
	(:use clojure.java.jdbc)
	(:require [clojure.string :as str])
	(:require [ring-debug-logging.core :refer [wrap-with-logger]])
)

(defn get-owner [db paper-id]
	(let [query-string "SELECT user_id FROM papers WHERE paper_id=?"]
		(get (first (query db [query-string paper-id])) :user_id)
	)
)

(defn get-new-id[result]
	(second (first (first result)))
)

(defn return-error [message]
	{:status 400 :body message}
)

(defn make-empty [text]
	(if text text "")
)

(defn get-user [db nameValue]
	(let [result (query db ["SELECT user_id,admin FROM users WHERE name =? AND valid;" nameValue])]
		(if (== 1 (count result))
			(first result)
			nil
		)
	)
)

(defn get-link [db link-id]
	(first (query db ["SELECT link_text, link FROM links WHERE link_id=?;" link-id]))
)

(defn get-reference-mapper [db]
	(fn [reference]
		{
			"reference_index" (get reference :reference_index)
			"link" (get-link db (get reference :link_id))
		}
	)
)

(defn get-references [db paper-id]
	(let 
		[
			query-string "SELECT reference_index,link_id FROM comment_references WHERE paper_id=?;"
			result (query db [query-string paper-id])
		]
			(doall (map (get-reference-mapper db) result))
	)
)

(defn get-votes [db paper-id]
	(let
		[
			columns " votes,users.name AS name"
			from " FROM votes JOIN users ON votes.user_id = users.user_id"
			where " WHERE votes.paper_id=?"
			query-string (str "SELECT" columns from where ";")
		]
			(doall (query db [query-string paper-id]))
	)
)

(defn make-paper-apply [db]
	(fn [paper]
		(let [paper-id (get paper :paper_id)]
			{
				"paper_id" paper-id
				"title" (get paper :title)
				"submitter" (get paper :submitter)
				"paper" (get-link db (get paper :link_id))
				"paper_comment" (get paper :paper_comment)
				"created_at" (get paper :created_at)
				"references" (get-references db paper-id)
				"votes" (get-votes db paper-id)
			}
		)
	)
)

(defn get-paper-list [db] 
	(let [
			columns "paper_id, title, link_id, paper_comment, created_at, users.name AS submitter"
			from " FROM papers JOIN users ON papers.user_id = users.user_id"
			where " paper_id NOT IN (SELECT paper_id FROM closed)"
			query-string (str "SELECT " columns from " WHERE" where	";")
			result (query db [query-string])
			map-fn (make-paper-apply db)
		]
		(doall(map map-fn result))
	)
)

(defn reload [db]
	{:body {:paper_list (get-paper-list db)}}
)

(defn total-votes [db user-id]
	(let
		[
			column "SUM(votes.votes) AS total_votes "
			tables "papers JOIN votes ON papers.paper_id=votes.paper_id "
			where "papers.paper_id NOT IN (SELECT paper_id FROM closed) AND votes.user_id=?;"
			query-string (str "SELECT " column "FROM " tables "WHERE " where)
			count-list (query db [query-string user-id])
			vote-count (get (first count-list) :total_votes)
		]

		(if (some? vote-count)
			vote-count
			0
		)
	)
)

(defn cast-new-vote [db user-id paper-id]
	(insert! db :votes {:paper_id paper-id, :user_id user-id, :votes 1})
	(reload db)
)

(defn cast-vote [db user-id paper-id votes]
	(update! db :votes {:votes (+ votes 1)} ["user_id=? AND paper_id=?" user-id paper-id])
	(reload db)
)

(defn uncast-vote [db user-id paper-id votes]
	(update! db :votes {:votes (- votes 1)} ["user_id=? AND paper_id=?" user-id paper-id])
	(reload db)
)

(defn get-vote-entry [db user-id paper-id]
	(let
		[
			where "user_id=? AND paper_id=?"
			vote-query (str "SELECT votes FROM votes WHERE " where ";")
		]
		(query db [vote-query user-id paper-id])
	)
)

(defn vote-for-paper [db user-id paper-id]
	(let
		[
			max-query "SELECT max_votes_per_paper FROM config WHERE config_id=1;"
			max-per-paper (get (first (query db [max-query])) :max_votes_per_paper)

			vote-entry (get-vote-entry db user-id paper-id)
		]
		(if (== (count vote-entry) 0)
			(cast-new-vote db user-id paper-id)
			(let [votes (get (first vote-entry) :votes)]
				(if (< votes max-per-paper)
					(cast-vote db user-id paper-id votes)
					(return-error "User has no votes left for this paper")
				)
			)
		)
	)
)

(defn unvote [db user-id paper-id]
	(if (some? user-id)
		(let [vote-entry (get-vote-entry db user-id paper-id)]
			(if (> (count vote-entry) 0)
				(let [votes (get (first vote-entry) :votes)]
					(if (> votes 0)
						(uncast-vote db user-id paper-id votes)
						(return-error "Cannot reduce votes to less than 0")
					)
				)
				(return-error "Cannot reduce votes to less than 0")
			)
		)
		(return-error  "User id not found")
	)
)

(defn vote [db user-id paper-id]
	(if (some? user-id)
		(let 
			[
				max-query "SELECT max_votes FROM config WHERE config_id=1;"
				max (get (first (query db [max-query])) :max_votes)
			]

			(if (> max (total-votes db user-id))
				(vote-for-paper db user-id paper-id)
				(return-error "User has used up his total votes")
			)
		)
		(return-error  "User id not found")
	)
)

(defn make-add-reference [db paper-id]
	(fn [reference]
		(insert! db :comment_references
			{
				"paper_id" paper-id
				,"reference_index" (get reference "index")
				,"link_id" (get-new-id (insert! db :links (get reference "link")))
			}
		)
	)
)

(defn add-reference-list [db paper-id reference-list]
	(doall(map (make-add-reference db paper-id) reference-list))
)

(defn add-a-paper [db user-id paper]
	(let [
			link-id (get-new-id (insert! db :links (get paper "paper")))
			paper-record 
				{
					"user_id" user-id
					,"title" (get paper "title")
					, "link_id" link-id
					, "paper_comment" (make-empty (get paper "comment"))
				}
		]
			
		(add-reference-list db (get-new-id (insert! db :papers paper-record)) (get paper "references"))
	)

	(reload db)
)

(defn can-add-paper [db user-id]
	(let 
		[
			count-query "SELECT COUNT(*) AS paper_count FROM papers WHERE open_paper AND user_id=?;"
			paper-count (get (first (query db [count-query user-id])) :paper_count)
			max-query "SELECT max_papers FROM config WHERE config_id=1;"
			max (get (first (query db [max-query])) :max_papers)
		]
		(< paper-count max)
	)
)

(defn add-paper [db user-id paper]
	(if (can-add-paper db user-id)
		(add-a-paper db user-id paper)
		(return-error "User cannot add any more papers")
	)
)

(defn update-a-paper [db paper-id paper]
	(let 
	[
		query-string "SELECT link_id FROM papers WHERE paper_id=?"
		old-link-id (get (first (query db [query-string paper-id])) :link_id)
		new-link-id (get-new-id (insert! db :links (get paper "paper")))
		paper-record 
			{
				,"title" (get paper "title")
				, "link_id" new-link-id
				, "paper_comment" (make-empty (get paper "comment"))
			}
	]

		(update! db :papers paper-record ["paper_id = ?" paper-id])
		(delete! db :links ["link_id = ?" old-link-id])
		(delete! db :comment_references ["paper_id = ?" paper-id])
		(add-reference-list db paper-id (get paper "references"))

		(reload db)
	)	
)

(defn update-paper [db user-id paper-id paper]
	(if (== (get-owner db paper-id) user-id)
		(update-a-paper db paper-id paper)
		(return-error "User did not submit paper")
	)
)

(defn edit-paper [db user-id paper]
	(if (some? user-id)
		(let [paper-id (get paper "paper_id")]
			(if (== paper-id 0)
				(add-paper db user-id paper)
				(update-paper db user-id paper-id paper)
			)
		)
		(return-error  "User id not found")
	)
)

(defn close-paper [db paper-id]
	(insert! db :closed {:paper_id paper-id})
)

(defn unclose-paper [db paper-id]
	(delete! db :closed ["paper_id=?" paper-id])
)

(defn user-close [db paper-id]
	(close-paper db paper-id)
	(reload db)
)

(defn close [db user-id paper-id]
	(if (some? user-id)
		(if (== (get-owner db paper-id) user-id)
			(user-close db paper-id)
			(return-error  "User did not submit paper")
		)
		(return-error  "Session not found")
	)
)

(defn login [db user-name]
	(let 
		[
			user (get-user db user-name)
			admin (get user :admin)
		]
		(if (some? user)
			{:body {:admin admin} :session user}
			(return-error "Invalid user")
		)
	)
)

(defn rules [db]
	(let 
		[
			columns "max_papers,max_votes,max_votes_per_paper"
			query-string (str "SELECT " columns " FROM config WHERE config_id=1")
			record (first (query db [query-string]))
		]
		{:body record}
	)
)

(defn return-user-list [db]
	{:body {:user_list (query db ["SELECT user_id,name,valid,admin FROM users"])}}
)

(defn add-user [db user-record]
	(insert! db :users user-record)
	(return-user-list db)
)

(defn change-user [db user-id user-record]
	(update! db :users user-record ["user_id=?" user-id])
	(return-user-list db)
)

(defn make-user-record [user]
	{
		:name (get user "name")
		:admin (get user "admin")
		:valid (get user "valid")
	}
)

(defn edit-user [db user]
	(let [user-id (get user "user_id")]
		(if (= user-id 0)
			(add-user db (make-user-record user))
			(change-user db user-id (make-user-record user))
		)
	)
)

(defn update-user [db admin user]
	(if (some? admin)
		(if admin
			(edit-user db user)
			(return-error "User not an administrator")
		)
		(return-error  "Session not found")
	)
)

(defn return-open-list [db]
	(let
		[
			id-column "papers.paper_id AS paper_id,"
			title-column "papers.title AS title,"
			comment-column "papers.paper_comment AS paper_comment,"
			votes-column "coalesce(sum(votes.votes),0) AS total_votes "

			columns (str "SELECT " id-column title-column comment-column votes-column)
			
			from "FROM papers LEFT OUTER JOIN votes ON papers.paper_id=votes.paper_id "

			where "WHERE papers.paper_id NOT IN (SELECT paper_id FROM closed) "

			group "GROUP BY papers.paper_id"
		]

		{:body {:paper_list (query db [(str columns from where group ";")])}}
	)
)

(defn return-closed-list [db]
	(let 
		[
			closed "closed.paper_id AS paper_id,closed.closed_at AS closed_at,"
			paper "papers.title AS title,papers.paper_comment AS paper_comment"
			columns (str closed paper)
			tables "closed INNER JOIN papers ON closed.paper_id = papers.paper_id"
		]
		{:body {:paper_list (query db [(str "SELECT " columns " FROM " tables ";")])}}
	)
)

(defn admin-close [db paper-id]
	(close-paper db paper-id)
	(return-open-list db)
)

(defn admin-unclose [db paper-id]
	(unclose-paper db paper-id)
	(return-closed-list db)
)

(defn admin-paper [action-fn db admin paper-id]
	(if (some? admin)
		(if admin
			(action-fn db paper-id)
			(return-error "User not an administrator")
		)
		(return-error  "Session not found")
	)
)

(defn admin-list [list-fn db admin]
	(if (some? admin)
		(if admin
			(list-fn db)
			(return-error "User not an administrator")
		)
		(return-error  "Session not found")
	)
)

(defn make-rules-record [rules]
	{
		:max_papers (get rules "max_papers")
		:max_votes (get rules "max_votes")
		:max_votes_per_paper (get rules "max_per_paper")
	}
)

(defn good-rules [db record]
	(update! db :config record ["config_id=1"])
	{:body record}
)

(defn replace-rules [db rules]
	(let [record (make-rules-record rules)]
		(if ( >= (get record :max_votes) (get record :max_votes_per_paper))
			(good-rules db record)
			(return-error "Max votes cannot be less than max votes per paper")
		)
	)
)

(defn update-rules [db rules admin]
	(if (some? admin)
		(if admin
			(replace-rules db rules)
			(return-error "User not an administrator")
		)
		(return-error  "Session not found")
	)
)

(defroutes voting
	(POST "/rules" [:as {db :connection}] (rules db))

	(POST "/login" [:as {db :connection {user "user"} :body}] (login db user))

	(POST "/reload" [:as {db :connection}] (reload db))

	(POST "/save" [:as {db :connection {paper "paper"} :body {user-id :user_id} :session}] 
		(edit-paper db user-id paper))

	(POST "/vote" [:as {db :connection {paper-id "paper_id"} :body {user-id :user_id} :session}] 
		(vote db user-id paper-id))

	(POST "/unvote" [:as {db :connection {paper-id "paper_id"} :body {user-id :user_id} :session}] 
		(unvote db user-id paper-id))

	(POST "/close" [:as {db :connection {paper-id "paper_id"} :body {user-id :user_id} :session}] 
		(close db user-id paper-id))

	(POST "/userList" [:as {db :connection {admin :admin} :session}] (admin-list return-user-list db admin))

	(POST "/updateUser" [:as {db :connection {user "user"} :body {admin :admin} :session}] 
		(update-user db admin user))

	(POST "/openList" [:as {db :connection {admin :admin} :session}] (admin-list return-open-list db admin))

	(POST "/adminClose" [:as {db :connection {paper-id "paper_id"} :body {admin :admin} :session}] 
		(admin-paper admin-close db admin paper-id))

	(POST "/closedList" [:as {db :connection {admin :admin} :session}] (admin-list return-closed-list db admin))

	(POST "/adminUnclose" [:as {db :connection {paper-id "paper_id"} :body {admin :admin} :session}] 
		(admin-paper admin-unclose db admin paper-id))

	(POST "/updateRules" [:as {db :connection {rules "rules"} :body {admin :admin} :session}]
		(update-rules db rules admin))
)

(defn make-wrap-db [db-url]
	(fn [handler]  
		(fn [req]   
			(with-db-connection [db {:connection-uri db-url}]
				(handler (assoc req :connection db))
			)
		)
	)
)

(defn cors [handler]
	(let [cors-policy
		    { 
		    	:allowed-origins :match-origin
				:allowed-methods #{:post}
				:request-headers #{"Accept" "Content-Type" "Origin"}
				:exposed-headers nil
				:allow-credentials? true
				:origin-varies? false
				:max-age nil
				:require-origin? true
				:ignore-failures? false
		    }
     	]

     	(wrap-cors handler cors-policy)
     )
)

(defn make-handler [db-url] 
	(let [wrap-db (make-wrap-db db-url)] 
		(-> voting
			(wrap-db)
			(wrap-json-body)
			(wrap-json-response)
			(wrap-session)
			(cors)
;			(wrap-with-logger)
		)
	)
)

(defn get-env [name]
	(let [value (System/getenv name)]
		(if (nil? value)
			(println (str "Evironment variable " name " is undefined"))
		)
		value
	)
)

(defn -main
  	"Cabal voting server"
  	[& args]
  	(if (== 0 (count args))
		(let [url (get-env "JDBC_DATABASE_URL") 
				portString (get-env "PORT")]
			(if (and (some? url) (some? portString))
				(try
					(let [port (Integer/parseInt portString)]
						(run-jetty (make-handler url) {:port port})
					)
					(catch NumberFormatException exception 
						(println (str portString " is not an int"))
					)
				)
			)
		)  	
	  	(println "This programme has no arguments")
	)
 )
