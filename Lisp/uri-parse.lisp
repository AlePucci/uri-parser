;;;; -*- Mode: Lisp -*-
;;;; uri-parse.lisp --

;;;; Alessandro Pucci 869177
;;;; Matias Aldo Ruiz 869139

;;; The purpose of this project is to create a library
;;; that can build structures, internally representing
;;; URIs (Universal Resource Identifiers), starting from their
;;; representations as strings.
;;; Parsing requires analyzing and decomposing the input string
;;; in sequence, character from character from left to right,
;;; in order to build a proper structure that stores its
;;; seven components.

;;; An URI is made of Scheme, Userinfo, Host, Port, Path,
;;; Query and Fragment.

;;; If not specified, in every scheme, 80 is the default port.
;;; An empty field, if the URI is still valid, is set to NIL.

;;; Def-port is set to 80 for every scheme.
(defparameter def-port 80)

;;; Uri-parse takes a string as input (an URI) and returns an
;;; "uri-structure", composed of the seven parts of URI, if valid.
;;; Otherwise, if not valid, it returns an error.
(defun uri-parse (uri-string)
  (if (stringp uri-string)
      (let ((uri (coerce uri-string 'list)))
        (let ((index-scheme (scheme uri 0)))
          (if (not (specialSchemep uri index-scheme))
              (if (checkAuth0 uri 0 index-scheme)
                  (let ((index-auth0 (auth0 uri 0 index-scheme)))
                    (if (checkUserinfo uri 0 index-auth0)
                        (let ((index-userinfo
			       (userinfo uri 0 index-auth0 0)))
                          (let ((index-host
				 (or
				  (host uri 0 index-userinfo 0)
				  (ip uri 0 index-userinfo 0 0 0))))
                            (if (checkPort uri 0 index-host)
                                (let ((index-port
				       (port
					uri
					0 
					(1+ index-host)
					0)))
                                  (if (checkSlash uri 0 index-port)
                                      (if (checkPath
					   uri 0 (1+ index-port))
                                          (let ((index-path
						 (path
						  uri 0 
						  (1+ index-port)
						  0)))
                                            (if (checkQuery
						 uri 0 index-path)
                                                (let
						    ((index-query 
						      (query
						       uri 0
						       (1+ index-path)
						       0)))
						  (if (checkFragment
						       uri
						       0
						       index-query)
						      (let
							  ((index-fragment
							    (fragment
							     uri
							     0 
							     (1+ index-query)
							     0)))
							;; SCHEME USERINFO HOST
							;; PORT / PATH QUERY
							;; FRAGMENT
							(list
							 (subseq
							  uri-string
							  0 
							  (1- index-scheme))
							 (subseq
							  uri-string
							  index-auth0 
							  (1- index-userinfo))
							 (subseq
							  uri-string
							  index-userinfo
							  index-host)
							 (parse-integer
							  (subseq 
							   uri-string 
							   (1+ index-host) 
							   index-port))
							 (subseq
							  uri-string
							  (1+ index-port)
							  index-path)
							 (subseq
							  uri-string
							  (1+ index-path)
							  index-query)
							 (subseq
							  uri-string
							  (1+ index-query)
							  index-fragment)))
						    ;; SCHEME USERINFO HOST
						    ;; PORT / PATH QUERY
                                                    (list
						     (subseq
						      uri-string
						      0 
						      (1- index-scheme))
						     (subseq
						      uri-string
						      index-auth0 
						      (1- index-userinfo))
						     (subseq
						      uri-string
						      index-userinfo
						      index-host)
						     (parse-integer
						      (subseq 
						       uri-string 
						       (1+ index-host) 
						       index-port))
						     (subseq
						      uri-string
						      (1+ index-port)
						      index-path)
						     (subseq
						      uri-string
						      (1+ index-path)
						      index-query)
						     NIL)))
                                              (if (checkFragment
						   uri
						   0
						   index-path)
                                                  (let
						      ((index-fragment
							(fragment
							 uri
							 0 
							 (1+ index-path)
							 0)))
                                                    ;; SCHEME USERINFO HOST
						    ;; PORT / PATH FRAGMENT
                                                    (list
						     (subseq
						      uri-string
						      0 
						      (1- index-scheme))
						     (subseq
						      uri-string
						      index-auth0 
						      (1- index-userinfo))
						     (subseq
						      uri-string
						      index-userinfo
						      index-host)
						     (parse-integer
						      (subseq 
						       uri-string 
						       (1+ index-host) 
						       index-port))
						     (subseq
						      uri-string
						      (1+ index-port)
						      index-path)
                                                     NIL
                                                     (subseq
						      uri-string
						      (1+ index-path)
                                                      index-fragment)))
                                                ;; SCHEME USERINFO HOST PORT
						;; / PATH
                                                (list (subseq
						       uri-string
						       0 
                                                       (1- index-scheme))
                                                      (subseq
						       uri-string
						       index-auth0 
                                                       (1- index-userinfo))
                                                      (subseq
						       uri-string
						       index-userinfo
                                                       index-host)
                                                      (parse-integer
						       (subseq
							uri-string 
						        (1+ index-host) 
							index-port))
                                                      (subseq
						       uri-string
						       (1+ index-port)
						       index-path)
                                                      NIL
                                                      NIL))))
                                        (if (checkQuery
					     uri
					     0
					     (1+ index-port))
                                            (let
						((index-query
						  (query 
                                                   uri
                                                   0
                                                   (+ 2 index-port)
                                                   0)))
                                              (if (checkFragment
						   uri
						   0
						   index-query)
                                                  (let
						      ((index-fragment
							(fragment
							 uri
							 0 
							 (1+ index-query)
							 0)))
                                                    ;; SCHEME USERINFO HOST
						    ;; PORT / QUERY FRAGMENT
                                                    (list
						     (subseq
						      uri-string
						      0 
						      (1- index-scheme))
						     (subseq
						      uri-string
						      index-auth0 
						      (1- index-userinfo))
						     (subseq
						      uri-string
						      index-userinfo
						      index-host)
						     (parse-integer
						      (subseq 
						       uri-string 
						       (1+ index-host) 
						       index-port))
                                                     NIL
						     (subseq
						      uri-string
						      (+ 2 index-port)
						      index-query)
                                                     (subseq
						      uri-string
						      (1+ index-query)
						      index-fragment)))
                                                ;; SCHEME USERINFO HOST
						;; PORT / QUERY
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
                                                 (subseq
						  uri-string
						  index-auth0 
						  (1- index-userinfo))
                                                 (subseq
						  uri-string
						  index-userinfo
						  index-host)
                                                 (parse-integer
						  (subseq 
						   uri-string 
						   (1+ index-host) 
						   index-port))
                                                 NIL
                                                 (subseq
						  uri-string
						  (+ 2 index-port)
						  index-query)
                                                 NIL)))
                                          (if (checkfragment
					       uri
					       0
					       (1+ index-port))
                                              (let 
						  ((index-fragment
						    (fragment
						     uri
						     0 
						     (+ 2 index-port)
						     0)))
                                                ;; SCHEME USERINFO HOST
						;; PORT / FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
                                                 (subseq
						  uri-string
						  index-auth0 
						  (1- index-userinfo))
                                                 (subseq
						  uri-string
						  index-userinfo
						  index-host)
                                                 (parse-integer
						  (subseq 
						   uri-string 
						   (1+ index-host) 
						   index-port))
                                                 NIL
                                                 NIL
                                                 (subseq
						  uri-string
						  (+ 2 index-port)
						  index-fragment)))
                                            ;; SCHEME USERINFO HOST PORT /
                                            (if (checkEnd
						 uri
						 0
						 (1+ index-port))
                                                (list (subseq
						       uri-string
						       0 
						       (1- index-scheme))
                                                      (subseq
						       uri-string
						       index-auth0 
						       (1- index-userinfo))
                                                      (subseq
						       uri-string
						       index-userinfo
						       index-host)
                                                      (parse-integer
						       (subseq 
							uri-string 
							(1+ index-host) 
							index-port))
                                                      NIL
                                                      NIL
                                                      NIL)
                                              (uri-error)))))
                                    ;; SCHEME USERINFO HOST PORT
                                    (if (checkEnd
					 uri
					 0
					 index-port)
                                        (list
					 (subseq
					  uri-string
					  0 
					  (1- index-scheme))
                                         (subseq
					  uri-string
					  index-auth0 
					  (1- index-userinfo))
                                         (subseq
					  uri-string
					  index-userinfo
					  index-host)
                                         (parse-integer
					  (subseq 
					   uri-string 
					   (1+ index-host) 
					   index-port))
                                         NIL
                                         NIL
                                         NIL)
                                      (uri-error))))
                              (if (checkSlash
				   uri
				   0
				   index-host)
                                  (if
				      (checkPath
				       uri
				       0
				       (1+ index-host))
                                      (let
					  ((index-path
					    (path
					     uri
					     0 
					     (1+ index-host)
					     0)))
                                        (if (checkQuery
					     uri
					     0
					     index-path)
                                            (let
						((index-query
						  (query
						   uri
						   0
						   (1+ index-path)
						   0)))
                                              (if
						  (checkFragment
						   uri
						   0
						   index-query)
                                                  (let
						      ((index-fragment
							(fragment
							 uri
							 0 
							 (1+ index-query)
							 0)))
                                                    ;; SCHEME USERINFO HOST /
						    ;; PATH QUERY FRAGMENT
                                                    (list (subseq
							   uri-string
							   0 
							   (1- index-scheme))
                                                          (subseq
							   uri-string
							   index-auth0 
							   (1- index-userinfo))
                                                          (subseq
							   uri-string
							   index-userinfo
							   index-host)
                                                          def-port
                                                          (subseq
							   uri-string
							   (1+ index-host)
							   index-path)
                                                          (subseq
							   uri-string
							   (1+ index-path)
							   index-query)
                                                          (subseq
							   uri-string
							   (1+ index-query)
							   index-fragment)))
                                                ;; SCHEME USERINFO HOST /
						;; PATH QUERY
                                                (list (subseq
						       uri-string
						       0 
						       (1- index-scheme))
                                                      (subseq
						       uri-string
						       index-auth0 
						       (1- index-userinfo))
                                                      (subseq
						       uri-string
						       index-userinfo
						       index-host)
                                                      def-port
                                                      (subseq
						       uri-string
						       (1+ index-host)
						       index-path)
                                                      (subseq
						       uri-string
						       (1+ index-path)
						       index-query)
                                                      NIL)))
                                          (if (checkFragment
					       uri
					       0
					       index-path)
                                              (let ((index-fragment
						     (fragment
						      uri
						      0 
						      (1+ index-path)
						      0)))
                                                ;; SCHEME USERINFO HOST /
						;; PATH FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
                                                 (subseq
						  uri-string
						  index-auth0 
						  (1- index-userinfo))
                                                 (subseq
						  uri-string
						  index-userinfo
						  index-host)
                                                 def-port
						 (subseq
						  uri-string
						  (1+ index-host)
						  index-path)
                                                 NIL
                                                 (subseq
						  uri-string
						  (1+ index-path)
						  index-fragment)))
                                            ;; SCHEME USERINFO HOST / PATH
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
                                             (subseq
					      uri-string
					      index-auth0 
					      (1- index-userinfo))
                                             (subseq
					      uri-string
					      index-userinfo
					      index-host)
                                             def-port
                                             (subseq
					      uri-string
					      (1+ index-host)
					      index-path)
                                             NIL
                                             NIL))))
                                    (if (checkQuery
					 uri
					 0
					 (1+ index-host))
                                        (let
					    ((index-query
					      (query
					       uri
					       0
					       (+ 2 index-host)
					       0)))
                                          (if (checkFragment
					       uri
					       0
					       index-query)
                                              (let
						  ((index-fragment
						    (fragment
						     uri
						     0 
						     (1+ index-query)
						     0)))
                                                ;; SCHEME USERINFO HOST /
						;; QUERY FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
						 (subseq
						  uri-string
						  index-auth0 
						  (1- index-userinfo))
						 (subseq
						  uri-string
						  index-userinfo
						  index-host)
						 def-port
						 NIL
						 (subseq
						  uri-string
						  (+ 2 index-host)
						  index-query)
						 (subseq
						  uri-string
						  (1+ index-query)
						  index-fragment)))
                                            ;; SCHEME USERINFO HOST / QUERY
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     (subseq
					      uri-string
					      index-auth0 
					      (1- index-userinfo))
                                             (subseq
					      uri-string
					      index-userinfo
					      index-host)
					     def-port
					     NIL
					     (subseq
					      uri-string
					      (+ 2 index-host)
					      index-query)
					     NIL)))
                                      (if (checkFragment
					   uri
					   0
					   (1+ index-host))
                                          (let
					      ((index-fragment
						(fragment
						 uri
						 0 
						 (+ 2 index-host)
						 0)))
                                            ;; SCHEME USERINFO HOST / FRAGMENT
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     (subseq
					      uri-string
					      index-auth0 
					      (1- index-userinfo))
					     (subseq
					      uri-string
					      index-userinfo
					      index-host)
					     def-port
					     NIL
					     NIL
					     (subseq
					      uri-string
					      (+ 2 index-host)
					      index-fragment)))
                                        ;; SCHEME USERINFO HOST  /
                                        (if (checkEnd
					     uri
					     0
					     (1+ index-host))
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     (subseq
					      uri-string
					      index-auth0 
					      (1- index-userinfo))
					     (subseq
					      uri-string
					      index-userinfo
					      index-host)
                                             def-port
                                             NIL
                                             NIL
                                             NIL)
                                          (uri-error)))))
                                ;; SCHEME USERINFO HOST
                                (if (checkEnd
				     uri
				     0
				     index-host)
                                    (list
				     (subseq
				      uri-string
				      0 
				      (1- index-scheme))
				     (subseq
				      uri-string
				      index-auth0 
				      (1- index-userinfo))
				     (subseq
				      uri-string
				      index-userinfo
				      index-host)
                                     def-port
                                     NIL
                                     NIL
                                     NIL)
                                  (uri-error))))))
                      (let ((index-host
			     (or
			      (host uri 0 index-auth0 0)
			      (ip uri 0 index-auth0 0 0 0))))
                        (if (checkPort
			     uri
			     0
			     index-host)
                            (let
				((index-port
				  (port
				   uri
				   0
				   (1+ index-host)
				   0)))
                              (if (checkSlash
				   uri
				   0
				   index-port)
                                  (if (checkPath
				       uri
				       0
				       (1+ index-port))
                                      (let
					  ((index-path
					    (path
					     uri
					     0 
					     (1+ index-port)
					     0)))
                                        (if (checkQuery
					     uri
					     0
					     index-path)
                                            (let
						((index-query
						  (query
						   uri
						   0
						   (1+ index-path)
						   0)))
                                              (if (checkFragment
						   uri
						   0
						   index-query)
                                                  (let
						      ((index-fragment
							(fragment
							 uri
							 0 
							 (1+ index-query)
							 0)))
                                                    ;; SCHEME HOST PORT /
						    ;; PATH QUERY FRAGMENT
                                                    (list
						     (subseq
						      uri-string
						      0 
						      (1- index-scheme))
						     NIL
						     (subseq
						      uri-string
						      index-auth0
						      index-host)
						     (parse-integer
						      (subseq 
						       uri-string 
						       (1+ index-host) 
						       index-port))
						     (subseq
						      uri-string
						      (1+ index-port)
						      index-path)
						     (subseq
						      uri-string
						      (1+ index-path)
						      index-query)
						     (subseq
						      uri-string
						      (1+ index-query)
						      index-fragment)))
                                                ;; SCHEME HOST PORT / PATH
						;; QUERY
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
						 NIL
						 (subseq
						  uri-string
						  index-auth0
						  index-host)
						 (parse-integer
						  (subseq 
						   uri-string 
						   (1+ index-host) 
						   index-port))
						 (subseq
						  uri-string
						  (1+ index-port)
						  index-path)
						 (subseq
						  uri-string
						  (1+ index-path)
						  index-query)
						 NIL)))
                                          (if (checkfragment
					       uri
					       0
					       index-path)
                                              (let
						  ((index-fragment
						    (fragment
						     uri
						     0 
						     (1+ index-path)
						     0)))			
                                                ;; SCHEME HOST PORT / PATH
						;; FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
						 NIL
						 (subseq
						  uri-string
						  index-auth0
						  index-host)
						 (parse-integer
						  (subseq 
						   uri-string 
						   (1+ index-host) 
						   index-port))
						 (subseq
						  uri-string
						  (1+ index-port)
						  index-path)
						 NIL
						 (subseq
						  uri-string
						  (1+ index-path)
						  index-fragment)))
                                            ;; SCHEME HOST PORT / PATH
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     NIL
					     (subseq
					      uri-string
					      index-auth0
					      index-host)
					     (parse-integer
					      (subseq 
					       uri-string 
					       (1+ index-host) 
					       index-port))
					     (subseq
					      uri-string
					      (1+ index-port)
					      index-path)
					     NIL
					     NIL))))
                                    (if (checkQuery
					 uri
					 0 (1+ index-port))
                                        (let
					    ((index-query
					      (query
					       uri
					       0
					       (+ 2 index-port)
					       0)))
                                          (if (checkFragment
					       uri
					       0
					       index-query)
                                              (let
						  ((index-fragment
						    (fragment
						     uri
						     0 
						     (1+ index-query)
						     0)))
                                                ;; SCHEME HOST PORT / QUERY
						;; FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
						 NIL
						 (subseq
						  uri-string
						  index-auth0
						  index-host)
						 (parse-integer
						  (subseq 
						   uri-string 
						   (1+ index-host) 
						   index-port))
                                                 NIL
						 (subseq
						  uri-string
						  (+ 2 index-port)
						  index-query)
                                                 (subseq
						  uri-string
						  (1+ index-query)
						  index-fragment)))
                                            ;; SCHEME HOST PORT / QUERY
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     NIL
					     (subseq
					      uri-string
					      index-auth0
					      index-host)
					     (parse-integer
					      (subseq 
					       uri-string 
					       (1+ index-host) 
					       index-port))
					     NIL
					     (subseq
					      uri-string
					      (+ 2 index-port)
					      index-query)
                                             NIL)))
                                      (if (checkfragment
					   uri
					   0
					   (1+ index-port))
                                          (let
					      ((index-fragment
						(fragment
						 uri
						 0 
						 (+ 2 index-port)
						 0)))
                                            ;; SCHEME HOST PORT / FRAGMENT
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     NIL
					     (subseq
					      uri-string
					      index-auth0
					      index-host)
					     (parse-integer
					      (subseq 
					       uri-string 
					       (1+ index-host) 
					       index-port))
                                             NIL
                                             NIL
                                             (subseq
					      uri-string
					      (+ 2 index-port)
					      index-fragment)))
                                        ;; SCHEME HOST PORT /
                                        (if (checkEnd
					     uri
					     0
					     (1+ index-port))
                                            (list
					     (subseq
					      uri-string
					      0 
					      (1- index-scheme))
					     NIL
					     (subseq
					      uri-string
					      index-auth0
					      index-host)
					     (parse-integer
					      (subseq 
					       uri-string 
					       (1+ index-host) 
					       index-port))
                                             NIL
                                             NIL
                                             NIL)
                                          (uri-error)))))
                                ;; SCHEME HOST PORT
                                (if (checkEnd
				     uri
				     0
				     index-port)
                                    (list
				     (subseq
				      uri-string
				      0 
				      (1- index-scheme))
				     NIL
				     (subseq
				      uri-string
				      index-auth0
				      index-host)
				     (parse-integer
				      (subseq 
				       uri-string 
				       (1+ index-host) 
				       index-port))
                                     NIL
                                     NIL
                                     NIL)
                                  (uri-error))))
                          (if (checkSlash
			       uri
			       0
			       index-host)
                              (if (checkPath
				   uri
				   0
				   (1+ index-host))
                                  (let
				      ((index-path
					(path
					 uri
					 0 
					 (1+ index-host)
					 0)))
                                    (if (checkQuery
					 uri
					 0
					 index-path)
                                        (let
					    ((index-query
					      (query
					       uri
					       0
                                               (1+ index-path)
					       0)))
                                          (if (checkFragment
					       uri
					       0
					       index-query)
                                              (let
						  ((index-fragment
						    (fragment
						     uri
						     0 
						     (1+ index-query)
						     0)))
                                                ;; SCHEME HOST / PATH QUERY
						;; FRAGMENT
                                                (list
						 (subseq
						  uri-string
						  0 
						  (1- index-scheme))
						 NIL
						 (subseq
						  uri-string
						  index-auth0
						  index-host)
						 def-port
						 (subseq
						  uri-string
						  (1+ index-host)
						  index-path)
						 (subseq
						  uri-string
						  (1+ index-path)
						  index-query)
						 (subseq
						  uri-string
						  (1+ index-query)
						  index-fragment)))
                                            ;; SCHEME HOST / PATH QUERY
                                            (list (subseq
						   uri-string
						   0 
                                                   (1- index-scheme))
                                                  NIL
                                                  (subseq
						   uri-string
						   index-auth0
						   index-host)
                                                  def-port
                                                  (subseq
						   uri-string
						   (1+ index-host)
                                                   index-path)
                                                  (subseq
						   uri-string
						   (1+ index-path)
						   index-query)
                                                  NIL)))
                                      (if (checkfragment
					   uri
					   0
					   index-path)
                                          (let ((index-fragment
						 (fragment
						  uri
                                                  0 
                                                  (1+ index-path)
                                                  0)))
                                            ;; SCHEME HOST / PATH FRAGMENT
                                            (list (subseq
						   uri-string
						   0 
						   (1- index-scheme))
                                                  NIL
                                                  (subseq
						   uri-string
						   index-auth0
						   index-host)
                                                  def-port
                                                  (subseq
						   uri-string
						   (1+ index-host)
						   index-path)
                                                  NIL
                                                  (subseq
						   uri-string
						   (1+ index-path)
						   index-fragment)))
                                        ;; SCHEME HOST / PATH
                                        (list (subseq
					       uri-string
					       0 
					       (1- index-scheme))
                                              NIL
                                              (subseq
					       uri-string
					       index-auth0
					       index-host)
                                              def-port
                                              (subseq
					       uri-string
					       (1+ index-host)
					       index-path)
                                              NIL
                                              NIL))))
                                (if (checkQuery
				     uri
				     0
				     (1+ index-host))
                                    (let ((index-query
					   (query
					    uri
					    0
					    (+ 2 index-host)
					    0)))
                                      (if (checkFragment
					   uri
					   0
					   index-query)
                                          (let ((index-fragment
						 (fragment
						  uri
						  0 
						  (1+ index-query)
						  0)))
                                            ;; SCHEME HOST / QUERY FRAGMENT
                                            (list
					     (subseq
					      uri-string
					      0
					      (1- index-scheme))
					     NIL
					     (subseq
					      uri-string
					      index-auth0
					      index-host)
					     def-port
					     NIL
					     (subseq
					      uri-string
					      (+ 2 index-host)
					      index-query)
					     (subseq
					      uri-string
					      (1+ index-query)
					      index-fragment)))
                                        ;; SCHEME HOST / QUERY
                                        (list
					 (subseq
					  uri-string
					  0 
					  (1- index-scheme))
					 NIL
					 (subseq
					  uri-string
					  index-auth0
					  index-host)
					 def-port
					 NIL
					 (subseq
					  uri-string
					  (+ 2 index-host)
					  index-query)
                                         NIL)))
                                  (if (checkfragment
				       uri
				       0
				       (1+ index-host))
                                      (let
					  ((index-fragment
					    (fragment
					     uri
					     0 
					     (+ 2 index-host)
					     0)))
                                        ;; SCHEME HOST / FRAGMENT
                                        (list
					 (subseq
					  uri-string
					  0 
					  (1- index-scheme))
					 NIL
					 (subseq
					  uri-string
					  index-auth0
					  index-host)
                                         def-port
                                         NIL
                                         NIL
                                         (subseq
					  uri-string
					  (+ 2 index-host)
					  index-fragment)))
                                    ;; SCHEME HOST  /
                                    (if (checkEnd
					 uri
					 0
					 (1+ index-host))
                                        (list
					 (subseq
					  uri-string
					  0 
					  (1- index-scheme))
                                         NIL
                                         (subseq
					  uri-string
					  index-auth0
					  index-host)
                                         def-port
                                         NIL
                                         NIL
                                         NIL)
                                      (uri-error)))))
                            ;; SCHEME HOST
                            (if (checkEnd
				 uri
				 0
				 index-host)
                                (list (subseq
				       uri-string
				       0 
				       (1- index-scheme))
                                      NIL
                                      (subseq
				       uri-string
				       index-auth0
				       index-host)
                                      def-port
                                      NIL
                                      NIL
                                      NIL)
                              (uri-error)))))))
                (if (checkSlash uri 0 index-scheme)
                    (if (checkPath uri 0
				   (1+ index-scheme))
                        (let ((index-path
			       (path uri 0 
				     (1+ index-scheme)
				     0)))
                          (if (checkQuery
			       uri 0 index-path)
                              (let ((index-query
				     (query
				      uri
				      0
				      (1+ index-path)
				      0)))
                                (if (checkFragment
				     uri
				     0
				     index-query)
                                    (let ((index-fragment
					   (fragment
					    uri
					    0
					    (1+ index-query)
					    0)))
                                      ;; SCHEME / PATH QUERY FRAGMENT
                                      (list
				       (subseq
					uri-string
					0 
					(1- index-scheme))
				       NIL
				       NIL
				       def-port
				       (subseq
					uri-string
					(1+ index-scheme)
					index-path)
                                       (subseq
					uri-string
					(1+ index-path)
					index-query)
                                       (subseq
					uri-string
					(1+ index-query)
					index-fragment)))
                                  ;; SCHEME / PATH QUERY
                                  (list
				   (subseq
				    uri-string
				    0 
				    (1- index-scheme))
                                   NIL
                                   NIL
                                   def-port
                                   (subseq
				    uri-string
				    (1+ index-scheme)
				    index-path)
                                   (subseq
				    uri-string
				    (1+ index-path)
				    index-query)
                                   NIL)))
                            (if (checkfragment
				 uri
				 0
				 index-path)
                                (let
				    ((index-fragment
				      (fragment
				       uri
				       0 
				       (1+ index-path)
				       0)))                                  
                                  ;; SCHEME / PATH FRAGMENT
                                  (list
				   (subseq uri-string 0 
                                           (1- index-scheme))
                                   NIL
                                   NIL
                                   def-port
                                   (subseq
				    uri-string
				    (1+ index-scheme)
                                    index-path)
                                   NIL
                                   (subseq
				    uri-string
				    (1+ index-path)
                                    index-fragment)))
                              ;; SCHEME / PATH
                              (list
			       (subseq
				uri-string 0 
                                (1- index-scheme))
                               NIL
                               NIL
                               def-port
                               (subseq
				uri-string
				(1+ index-scheme)
                                index-path)
                               NIL
                               NIL))))
                      (if (checkQuery uri 0 (1+ index-scheme))
                          (let ((index-query (query uri 0
                                                    (+ 2 index-scheme) 0)))
                            (if (checkFragment uri 0 index-query)
                                (let ((index-fragment
				       (fragment uri
                                                 0 
                                                 (1+ index-query)
                                                 0)))
                                  ;; SCHEME / QUERY FRAGMENT
                                  (list
				   (subseq
				    uri-string
				    0 
                                    (1- index-scheme))
                                   NIL
                                   NIL
                                   def-port
                                   NIL
                                   (subseq
				    uri-string
				    (+ 2 index-scheme)
                                    index-query)
                                   (subseq
				    uri-string
				    (1+ index-query)
                                    index-fragment)))
                              ;; SCHEME / QUERY
                              (list
			       (subseq
				uri-string
				0 
                                (1- index-scheme))
                               NIL
                               NIL
                               def-port
                               NIL
                               (subseq
				uri-string
				(+ 2 index-scheme)
                                index-query)
                               NIL)))
                        (if (checkfragment uri 0 (1+ index-scheme))
                            (let ((index-fragment (fragment uri
                                                            0 
                                                            (+ 2 index-scheme)
                                                            0)))
                              ;; SCHEME / FRAGMENT
                              (list (subseq
				     uri-string
				     0 
                                     (1- index-scheme))
                                    NIL
                                    NIL
                                    def-port
                                    NIL
                                    NIL
                                    (subseq
				     uri-string
				     (+ 2 index-scheme) 
                                     index-fragment)))
                          ;; SCHEME /
                          (if (checkEnd uri 0 (1+ index-scheme))
                              (list (subseq
				     uri-string
				     0 
                                     (1- index-scheme))
                                    NIL
                                    NIL
                                    def-port
                                    NIL
                                    NIL
                                    NIL)
                            (uri-error)))))
                  ;; SCHEME 
                  (if (checkEnd uri 0 index-scheme)
                      (list (subseq
			     uri-string
			     0 
                             (1- index-scheme))
                            NIL
                            NIL
                            def-port
                            NIL
                            NIL
                            NIL)
                    (uri-error))))
            ;; SPECIAL SCHEME SYNTAX
            (let ((b-scheme (subseq uri-string 0 (1- index-scheme))))
              (let ((scheme (string-downcase b-scheme)))
		(if (equal scheme "mailto")
                    (if (checkEnd uri 0 index-scheme)
			;; MAILTO :
			(list b-scheme
                              NIL
                              NIL
                              def-port
                              NIL
                              NIL
                              NIL)
                      (let ((index-userinfo
			     (userinfoSpecial
			      uri 0 index-scheme 0)))
			(if (checkSpecialHost
			     uri 0 (1- index-userinfo))
                            (let ((index-host
				   (or (hostSpecial
					uri 0 (1+ index-userinfo) 0)
				       (ip uri 0
					   (1+ index-userinfo) 0 0 0))))
                              (if (checkEnd
				   uri 0 index-host)
                                  ;; MAILTO : USERINFO @ HOST
                                  (list b-scheme
					(subseq
					 uri-string
					 index-scheme
					 index-userinfo)
					(subseq
					 uri-string
					 (1+ index-userinfo)
					 index-host)
					def-port
					NIL
					NIL
					NIL)
				(uri-error)))
                          ;; MAILTO : USERINFO
                          (list b-scheme
				(subseq
				 uri-string
				 index-scheme
				 index-userinfo)
				NIL
				def-port
				NIL
				NIL
				NIL))))
                  (if (equal scheme "news")
                      (if (checkEnd
			   uri
			   0
			   index-scheme)
                          ;; NEWS :
                          (list b-scheme
				NIL
				NIL
				def-port
				NIL
				NIL
				NIL)
			(let ((index-host
			       (or (hostSpecial
				    uri
				    0
				    index-scheme
				    0)
				   (ip
				    uri
				    0
				    index-scheme
				    0
				    0
				    0))))
                          ;; NEWS : HOST
                          (list b-scheme
				NIL
				(subseq
				 uri-string
				 index-scheme
				 index-host)
				def-port
				NIL
				NIL
				NIL)))
                    (if (or (equal scheme "tel")
                            (equal scheme "fax"))
			(if (checkEnd
			     uri
			     0
			     index-scheme)
                            ;; TEL/FAX :
                            (list
			     b-scheme
                             NIL
                             NIL
                             def-port
                             NIL
                             NIL
                             NIL)
                          (let
			      ((index-userinfo
				(userinfoSpecial2
				 uri
				 0
				 index-scheme
				 0)))
                            ;; TEL/FAX : USERINFO
                            (list b-scheme
                                  (subseq
				   uri-string
				   index-scheme
				   index-userinfo)
                                  NIL
                                  def-port
                                  NIL
                                  NIL
                                  NIL)))
                      (if (equal scheme "zos")
                          (parse-zos uri uri-string index-scheme)
			(uri-error))))))))))
    (error "URI is not a string!")))

;;; Returns scheme part of URI.
(defun uri-scheme (uri)
  (first uri))

;; Returns userinfo part of URI if exists (NIL, otherwise)
(defun uri-userinfo (uri)
  (second uri))

;;; Returns host part of URI if exists (NIL, otherwise)
(defun uri-host (uri)
  (third uri))

;;; Returns port part of URI if exists (def-port, otherwise)
;;; def-port is set to 80 for every scheme.
(defun uri-port (uri)
  (fourth uri))

;;; Returns path part of URI if exists (NIL, otherwise)
(defun uri-path (uri)
  (fifth uri))

;;; Returns query part of URI if exists (NIL, otherwise)
(defun uri-query (uri)
  (sixth uri))

;;; Returns fragment part of URI if exists (NIL, otherwise)
(defun uri-fragment (uri)
  (seventh uri))

;;; Calls uri-print with URI and an optional stream (default is "t")
;;; and returns true.
(defun uri-display (uri &optional (stream t))
  (let ((whatever (uri-print uri stream))) (null whatever) t))

;;; Prints URI in the provided stream, in a text format,
;;; then automatically closes the stream.
(defun uri-print (uri stream)
  (format stream "Scheme:     ~A~%"   (uri-scheme uri))
  (format stream "Userinfo:   ~A~%"   (uri-userinfo uri))
  (format stream "Host:       ~A~%"   (uri-host uri))
  (format stream "Port:       ~A~%"   (uri-port uri))
  (format stream "Path:       ~A~%"   (uri-path uri))
  (format stream "Query:      ~A~%"   (uri-query uri))
  (format stream "Fragment:   ~A~%~%" (uri-fragment uri)))

;;; Scheme takes URI and a start index as input and
;;; returns the last index of scheme part.
(defun scheme (uri index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp index) 
             (identifierp f-uri))
        (scheme r-uri (1+ index))
      (if (and (not (null f-uri))
               (integerp index)
               (>= index 1)
               (char= f-uri #\Colon))
          ;; index right after ":"
          (1+ index) 
        (uri-error)))))

;;; Returns true if the current scheme is special,
;;; i.e. t if it's "mailto", "news", "tel", "fax" or "zos".
;;; Scheme parsing is case-insensitive!
(defun specialSchemep (uri index)
  (let ((string-uri (coerce uri 'string)))
    (let ((scheme (string-downcase (subseq string-uri 0 (1- index)))))
      (or (equal scheme "mailto")
          (equal scheme "news")
          (equal scheme "tel")
          (equal scheme "fax")
          (equal scheme "zos")))))

;;; ACC starts from 0 and increases of 1 until INDEX. In this way 
;;; each function can continue parsing where it was left.
;;; In some functions another parameter is requested: MIN, it's
;;; used to force user to have more than 0 character in one
;;; of each part of URI. MIN increases of 1 every legit
;;; recursive call.

;;; Returns true if index refers to the last
;;; character of uri.
(defun checkEnd (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkEnd r-uri (1+ acc) index)
      (if (null f-uri)
          t))))

;;; Returns true if "//" is found after the index.
;;; i.e. if auth0 part exists.
(defun checkAuth0 (uri acc index)
  (let ((f1-uri (first uri)) (f2-uri (second uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkAuth0 r-uri (1+ acc) index)
      (if (and (not (null f1-uri))
               (not (null f2-uri))
               (integerp index)
               (char= f1-uri #\Solidus)
               (char= f2-uri #\Solidus))
          t))))

;;; Returns the index after the double slash.
(defun auth0 (uri acc index)
  (let ((f1-uri (first uri)) (f2-uri (second uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (auth0 r-uri (1+ acc) index)
      (if (and (not (null f1-uri))
               (not (null f2-uri))
               (integerp index)
               (char= f1-uri #\Solidus)
               (char= f2-uri #\Solidus))
          ;; index right after "//"
          (+ 2 index)
        (uri-error)))))

;;; Returns true if a "@" is found after the index.
;;; i.e. t if userinfo exists.
(defun checkUserinfo (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkUserinfo r-uri (1+ acc) index)
      (if (and (integerp acc)
               (integerp index)
               (identifierp f-uri))
          (checkUserinfo r-uri (1+ acc) (1+ index))
        (if (and (not (null f-uri))
                 (char= f-uri #\Commercial-At))
            t)))))

;;; Returns the last index of userinfo part.
(defun userinfo (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (userinfo r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (identifierp f-uri))
          (userinfo r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (not (null f-uri))
                 (integerp min)
                 (>= min 1)
                 (char= f-uri #\Commercial-At))
            ;; index right after "@"
            (1+ index) 
          (uri-error))))))

;;; Returns the last index of userinfo part in a special scheme.
(defun userinfoSpecial (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (userinfoSpecial r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (identifierp f-uri))
          (userinfoSpecial r-uri (1+ acc) (1+ index) (1+ min))
        (if (or (and (null f-uri)
                     (integerp min)
                     (>= min 1))
                (and (not (null f-uri))
                     (integerp min)
                     (>= min 1)
                     (char= f-uri #\Commercial-At)))
            index 
          (uri-error))))))

;;; Returns the last index of userinfo part in a special scheme.
(defun userinfoSpecial2 (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (userinfoSpecial2 r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (identifierp f-uri))
          (userinfoSpecial2 r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (null f-uri)
                 (integerp min)
                 (>= min 1))
            index 
          (uri-error))))))

;;; Returns true if a "@" is found after the index.
;;; i.e. t if host exists in a special scheme.
(defun checkSpecialHost (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkSpecialHost r-uri (1+ acc) index)
      (if (and (integerp acc)
               (integerp index)
               (identifierp f-uri))
          (checkSpecialHost r-uri (1+ acc) (1+ index))
        (if (and (not (null f-uri))
                 (char= f-uri #\Commercial-At))
            t)))))

;;; Returns the last index of host part.
(defun host (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (host r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (host-identifierp f-uri))
          (host r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (not (null f-uri))
                 (integerp acc)
                 (integerp index)
                 (integerp min)
                 (>= min 1)
                 (char= f-uri #\Full-Stop))
            ;; In this case min is set to 0
            ;; in order to have at least 1
            ;; character after "."
            (host r-uri (1+ acc) (1+ index) 0)
          (if (or (and (not (null f-uri))
                       (integerp min)
                       (>= min 1)
                       (char= f-uri #\Colon))
                  (and (not (null f-uri))
                       (integerp min)
                       (>= min 1)
                       (char= f-uri #\Solidus))
                  (and (null uri)
                       (integerp min)
                       (>= min 1)))
              index
            (uri-error)))))))

;;; Returns the last index of host part in a special scheme.
(defun hostSpecial (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (hostSpecial r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (host-identifierp f-uri))
          (hostSpecial r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (not (null f-uri))
                 (integerp acc)
                 (integerp index)
                 (integerp min)
                 (>= min 1)
                 (char= f-uri #\Full-Stop))
            ;; In this case min is set to 0
            ;; in order to have at least 1
            ;; character after "."
            (hostSpecial r-uri (1+ acc) (1+ index) 0)
          (if  (and (null uri)
                    (integerp min)
                    (>= min 1))
              index
            (uri-error)))))))

;;; Returns the last index of ip (host part).
;;; IP (v4) adresseses must be in the following form:
;;; NNN.NNN.NNN.NNN (with 0 <= NNN <= 255)
(defun ip (uri acc index min max dots)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (ip r-uri (1+ acc) index min max dots)
      (if (and (not (null f-uri))
               (integerp acc)
               (integerp index)
               (integerp min)
               (= min 0)
               (char>= f-uri #\3)
               (char<= f-uri #\9))
          (ip r-uri (1+ acc) (1+ index) (1+ min) 2 dots)
        (if (and (not (null f-uri))
                 (integerp acc)
                 (integerp index)
                 (integerp min)
                 (= min 0)
                 (char>= f-uri #\0)
                 (char<= f-uri #\1))
            (ip r-uri (1+ acc) (1+ index) (1+ min) 3 dots)
          (if (and (not (null f-uri))
                   (integerp acc)
                   (integerp index)
                   (integerp min)
                   (= min 0)
                   (char= f-uri #\2))
              ;; In this case, max is set to 4
              ;; in order to recognize that the octet starts
              ;; with a 2.
              (ip r-uri (1+ acc) (1+ index) (1+ min) 4 dots)
            (if (and (not (null f-uri))
                     (integerp acc)
                     (integerp index)
                     (integerp min)
                     (= min 1)
                     (= max 2)
                     (digitp f-uri))
                (ip r-uri (1+ acc) (1+ index) (1+ min) 2 dots)
              (if (and (not (null f-uri))
                       (integerp acc)
                       (integerp index)
                       (integerp min)
                       (>= min 1)
                       (= max 3)
                       (digitp f-uri))
                  (ip r-uri (1+ acc) (1+ index) (1+ min) 3 dots)
                (if (and (not (null f-uri))
                         (integerp acc)
                         (integerp index)
                         (integerp min)
                         (= min 1)
                         (= max 4)
                         (char>= f-uri #\0)
                         (char<= f-uri #\4))
                    (ip r-uri (1+ acc) (1+ index) (1+ min) 3 dots)
                  (if (and (not (null f-uri))
                           (integerp acc)
                           (integerp index)
                           (integerp min)
                           (= min 1)
                           (= max 4)
                           (char= f-uri #\5))
                      (ip r-uri (1+ acc) (1+ index) (1+ min) 4 dots)
                    (if (and (not (null f-uri))
                             (integerp acc)
                             (integerp index)
                             (integerp min)
                             (= min 1)
                             (= max 4)
                             (char>= f-uri #\6)
                             (char<= f-uri #\9))
                        (ip r-uri (1+ acc) (1+ index) (1+ min) 2 dots)
                      (if (and (not (null f-uri))
                               (integerp acc)
                               (integerp index)
                               (integerp min)
                               (= min 1)
                               (= max 4)
                               (char>= f-uri #\0)
                               (char<= f-uri #\4))
                          (ip r-uri (1+ acc) (1+ index) (1+ min) 3 dots)
                        (if (and (not (null f-uri))
                                 (integerp acc)
                                 (integerp index)
                                 (integerp min)
                                 (= min 2)
                                 (= max 4)
                                 (char>= f-uri #\0)
                                 (char<= f-uri #\5))
                            (ip r-uri (1+ acc) (1+ index) (1+ min) 3 dots)
                          (if (and (not (null f-uri))
                                   (integerp acc)
                                   (integerp index)
                                   (integerp min)
                                   (integerp dots)
                                   (< dots 3)
                                   (>= min 1)
                                   (<= min max)
                                   (char= f-uri #\Full-Stop))
                              ;; In this case min is set to 0
                              ;; in order to have at least 1
                              ;; character after "." and the
                              ;; number of dots is increased by 1.
                              (ip r-uri (1+ acc) (1+ index) 0 max (1+ dots))
                            (if (or (and (not (null f-uri))
                                         (integerp min)
                                         (integerp dots)
                                         (>= min 1)
                                         (<= min max)
                                         (= dots 3)
                                         (char= f-uri #\Colon))
                                    (and (not (null f-uri))
                                         (integerp min)
                                         (integerp dots)
                                         (>= min 1)
                                         (<= min max)
                                         (= dots 3)
                                         (char= f-uri #\Solidus))
                                    (and (null uri)
                                         (integerp min)
                                         (integerp dots)
                                         (= dots 3)
                                         (>= min 1)
                                         (<= min max)))
                                index
                              (uri-error))))))))))))))))

;;; Returns true if a ":" is found after the index.
;;; i.e. t if port exists.
(defun checkPort (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkPort r-uri (1+ acc) index)
      (if (and (not (null f-uri))
               (char= f-uri #\Colon))
          t))))

;;; Returns the last index of port part.
(defun port (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (port r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (digitp f-uri))
          (port r-uri (1+ acc) (1+ index) (1+ min))
        (if (or (and (not (null f-uri))
                     (integerp min)
                     (>= min 1)
                     (char= f-uri #\Solidus))
                (and (null f-uri)
                     (integerp min)
                     (>= min 1)))
            index
          (uri-error))))))

;;; Returns true if a "/" is found after the index.
(defun checkSlash (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkSlash r-uri (1+ acc) index)
      (if (and (not (null f-uri))
               (char= f-uri #\Solidus))
          t))))

;;; Returns true if some path is found after the index.
;;; i.e. t if path exists.
(defun checkPath (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkPath r-uri (1+ acc) index)
      (if (and (not (null f-uri))
               (identifierp f-uri))
          t))))

;;; Returns the last index of path part.
(defun path (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (path r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (identifierp f-uri))
          (path r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (not (null f-uri))
                 (integerp acc)
                 (integerp index)
                 (integerp min)
                 (>= min 1)
                 (char= f-uri #\Solidus))
            ;; In this case min is set to 0
            ;; in order to have at least 1
            ;; character after "/"
            (path r-uri (1+ acc) (1+ index) 0)
          (if (or (and (not (null f-uri))
                       (integerp min)
                       (>= min 1)
                       (char= f-uri #\Question-Mark))
                  (and (not (null f-uri))
                       (integerp min)
                       (>= min 1)
                       (char= f-uri #\Number-Sign))
                  (and (null uri)
                       (integerp min)
                       (>= min 1)))
              index
            (uri-error)))))))

;;; Returns the last index of path part in a zos scheme.
(defun path-zos (uri acc index min44 min8 dot)
  ;; min8 is called firstly with -1.
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (path-zos r-uri (1+ acc) index min44 min8 dot)
      (if (and (not (null f-uri))
               (integerp acc)
               (integerp index)
               (integerp min44)
               (integerp min8)
               (= min8 -1)
               (id44 f-uri min44)
               (char= f-uri #\Full-Stop))
          (path-zos r-uri (1+ acc) (1+ index) (1+ min44) min8 1)
        (if (and (not (null f-uri))
                 (integerp acc)
                 (integerp index)
                 (integerp min44)
                 (integerp min8)
                 (= min8 -1)
                 (id44 f-uri min44)
                 (char/= f-uri #\Full-Stop))
            (path-zos r-uri (1+ acc) (1+ index) (1+ min44) min8 0)
          (if (and (not (null f-uri))
                   (integerp acc)
                   (integerp index)
                   (integerp min44)
                   (integerp min8)
                   (>= min44 1)
                   (= min8 -1)
                   (= dot 0)
                   (char= f-uri #\Left-Parenthesis))
              ;; In this case min8 is set to 0
              ;; in order to know that id44 part is finished.
              (path-zos r-uri (1+ acc) (1+ index) min44 0 dot)
            (if (and (integerp acc)
                     (integerp index)
                     (integerp min44)
                     (integerp min8)
                     (>= min44 1)
                     (>= min8 0)
                     (id8 f-uri min8))
                (path-zos r-uri (1+ acc) (1+ index) min44 (1+ min8) dot)
              (if (and (not (null f-uri))
                       (integerp min44)
                       (integerp min8)
                       (>= min44 1)
                       (<= min44 44)
                       (>= min8 1)
                       (<= min8 8)
                       (char= f-uri #\Right-Parenthesis))
                  (path-zos r-uri (1+ acc) (1+ index) min44 -2 dot)
                (if (and (null f-uri)
                         (integerp min44)
                         (integerp min8)
                         (>= min44 1)
                         (<= min44 44)
                         (<= min8 -1)
                         (= dot 0))
                    index
                  (if (and (not (null f-uri))
                           (integerp min44)
                           (integerp min8)
                           (>= min44 1)
                           (<= min44 44)
                           (<= min8 -1)
                           (= dot 0)
                           (char= f-uri #\Question-Mark))
                      index
                    (if (and (not (null f-uri))
                             (integerp min44)
                             (integerp min8)
                             (>= min44 1)
                             (<= min44 44)
                             (<= min8 -1) 
                             (= dot 0)
                             (char= f-uri #\Number-Sign))
                        index
                      (uri-error))))))))))))

;;; Returns true if a "?" is found after the index.
;;; i.e. t if query exists.
(defun checkQuery (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkQuery r-uri (1+ acc) index)
      (if (and (not (null f-uri))
               (char= f-uri #\Question-Mark))
          t))))

;;; Returns the last index of query part.
(defun query (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (query r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (query-identifierp f-uri))
          (query r-uri (1+ acc) (1+ index) (1+ min))
        (if (or (and (not (null f-uri))
                     (integerp min)
                     (>= min 1)
                     (char= f-uri #\Number-Sign))
                (and (null uri)
                     (integerp min)
                     (>= min 1)))
            index
          (uri-error))))))

;;; Returns true if a "#" is found after the index.
;;; i.e. t if fragment exists.
(defun checkFragment (uri acc index)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (checkFragment r-uri (1+ acc) index)
      (if (and (not (null f-uri))
               (char= f-uri #\Number-Sign))
          t))))

;;; Returns the last index of fragment part.
(defun fragment (uri acc index min)
  (let ((f-uri (first uri)) (r-uri (rest uri)))
    (if (and (integerp acc)
             (integerp index)
             (< acc index))
        (fragment r-uri (1+ acc) index min)
      (if (and (integerp acc)
               (integerp index)
               (integerp min)
               (fragment-identifierp f-uri))
          (fragment r-uri (1+ acc) (1+ index) (1+ min))
        (if (and (null uri)
                 (integerp min)
                 (>= min 1))
            index
          (uri-error))))))

;;; The following are all of the characters accepted in a URI,
;;; according to RFC (Request For Comment) 3986, section 2
;;; "Characters" :
;;; https://datatracker.ietf.org/doc/html/rfc3986#section-2
;;; identifierp, host-identifierp, query-identifierp,
;;; fragment-identifierp and path-zos-identifierp
;;; are subsets of it, depending on the list of accepted characters
;;; in each part of an Universal Resource Identifier.

;;; Returns true if the provided char (alpha) is a valid
;;; US-ASCII alphabetic value.
(defun alphap (alpha)
  (and (not (null alpha))
       (or  (and (char>= alpha #\a)
                 (char<= alpha #\z))
            (and (char>= alpha #\A)
                 (char<= alpha #\Z)))))

;;; Returns true if the provided char (alpha) is a valid
;;; US-ASCII alphanumeric value.
(defun alphanump (alpha)
  (and (not (null alpha))
       (or (alphap alpha)
           (digitp alpha))))

;;; Returns true if the provided char is an unreserved char
;;; according to RFC 3986.
(defun unreservedp (char)
  (and (not (null char))
       (or (alphanump char)
           (char= char #\-)
           (char= char #\Full-Stop)
           (char= char #\Low-Line)
           (char= char #\Tilde)
           (char= char #\Percent-Sign))))

;;; Returns true if the provided char is a reserved char
;;; according to RFC 3986.
(defun reservedp (char)
  (and (not (null char))
       (or (gen-delimsp char)
           (sub-delimsp char))))

;;; Returns true if the provided char is a valid identifier.
(defun identifierp (char)
  (and (not (null char))
       (or (unreservedp char)
           (sub-delimsp char)
           (char= char #\Left-Square-Bracket)
           (char= char #\Right-Square-Bracket))))

;;; Returns true if the provided char is a valid host identifier.             
(defun host-identifierp (char)
  (and (not (null char))
       (or (alphanump char)
           (sub-delimsp char)
           (char= char #\-)
           (char= char #\Low-Line)
           (char= char #\Tilde)
           (char= char #\Percent-Sign)
           (char= char #\Left-Square-Bracket)
           (char= char #\Right-Square-Bracket))))

;;; Returns true if the provided char is a valid query identifier.
(defun query-identifierp (char)
  (and (not (null char))
       (or (unreservedp char)
           (sub-delimsp char)
           (char= char #\Colon)
           (char= char #\Solidus)
           (char= char #\Question-Mark)
           (char= char #\Commercial-At)
           (char= char #\Left-Square-Bracket)
           (char= char #\Right-Square-Bracket))))

;;; Returns true if the provided char is a valid fragment identifier.
(defun fragment-identifierp (char)
  (and (not (null char))
       (or (unreservedp char)
           (reservedp char))))

;;; Returns true if the provided char is a valid id44 character
;;; in the zos path.
(defun id44 (char min)
  (or (and (not (null char))
           (integerp min)
           (= min 0)
           (alphap char))
      (and (not (null char))
           (integerp min)
           (> min 0)
           (or (alphanump char)
               (char= char #\Full-Stop)))))

;;; Returns true if the provided char is a valid id8 character
;;; in the zos path.
(defun id8 (char min)
  (or (and (not (null char))
           (integerp min)
           (= min 0)
           (alphap char))
      (and (not (null char))
           (integerp min)
           (> min 0)
           (alphanump char))))

;;; Returns true if the provided char is a gen-delim char
;;; according to RFC 3986.
(defun gen-delimsp (char)
  (and (not (null char))
       (or (char= char #\Colon)
           (char= char #\Solidus)
           (char= char #\Question-Mark)
           (char= char #\Number-Sign)
           (char= char #\Left-Square-Bracket)
           (char= char #\Right-Square-Bracket)
           (char= char #\Commercial-At))))

;;; Returns true if the provided char is a sub-delim char
;;; according to RFC 3986.
(defun sub-delimsp (char)
  (and (not (null char))
       (or (char= char #\Exclamation-Mark)
           (char= char #\Dollar-Sign)
           (char= char #\Ampersand)
           (char= char #\Apostrophe)
           (char= char #\Left-Parenthesis)
           (char= char #\Right-Parenthesis)
           (char= char #\Asterisk)
           (char= char #\Plus-Sign)
           (char= char #\Comma)
           (char= char #\Semicolon)
           (char= char #\Equals-Sign))))

;;; Returns true if the character provided (digit) represents a 
;;; valid digit.
(defun digitp (digit)
  (and (not (null digit))
       (or (char= digit #\0)
           (char= digit #\1)
           (char= digit #\2)
           (char= digit #\3)
           (char= digit #\4)
           (char= digit #\5)
           (char= digit #\6)
           (char= digit #\7)
           (char= digit #\8)
           (char= digit #\9))))

;;; Returns to the main function URI parts in case of
;;; a "zos" special scheme.
(defun parse-zos (uri uri-string index-scheme)
  (if (checkAuth0
       uri
       0
       index-scheme)
      (let ((index-auth0
	     (auth0
	      uri
	      0
	      index-scheme)))
        (if (checkUserinfo
	     uri
	     0
	     index-auth0)
            (let ((index-userinfo
		   (userinfo
		    uri
		    0
		    index-auth0
		    0)))
              (let ((index-host
		     (or
		      (host
		       uri
		       0
		       index-userinfo
		       0)
		      (ip
		       uri
		       0
		       index-userinfo
		       0 0 0))))
                (if (checkPort
		     uri
		     0
		     index-host)
                    (let ((index-port
			   (port
			    uri
			    0
			    (1+ index-host)
			    0)))
                      (if (checkSlash
			   uri
			   0
			   index-port)
			  (if (checkPath
			       uri
			       0
			       (1+
				index-port))
			      (let
				  ((
				    index-path
				    (path-zos
				     uri
				     0 
				     (1+
				      index-port)
				     0 -1 0)))
                                (if (checkQuery
				     uri
				     0
				     index-path)
                                    (let
					((index-query
					  (query
					   uri
					   0
					   (1+
					    index-path)
					   0)))
                                      (if (checkFragment
					   uri
					   0
					   index-query)
                                          (let
					      ((index-fragment
						(fragment
						 uri
						 0 
						 (1+
						  index-query)
						 0)))
                                            ;; ZOS USERINFO HOST
					    ;; PORT / PATH QUERY
					    ;; FRAGMENT
                                            (list
					     (subseq
					      uri-string
					      0
                                              (1-
					       index-scheme))
                                             (subseq
					      uri-string
					      index-auth0 
                                              (1-
					       index-userinfo))
                                             (subseq
					      uri-string
					      index-userinfo
					      index-host)
                                             (parse-integer
					      (subseq
					       uri-string 
                                               (1+
						index-host)
					       index-port))
                                             (subseq
					      uri-string
					      (1+
					       index-port)
                                              index-path)
                                             (subseq
					      uri-string
					      (1+
					       index-path)
                                              index-query)
                                             (subseq
					      uri-string
					      (1+
					       index-query)
                                              index-fragment)))
                                        ;; ZOS USERINFO HOST PORT
					;; / PATH QUERY
                                        (list
					 (subseq
					  uri-string
					  0 
                                          (1- index-scheme))
                                         (subseq
					  uri-string
					  index-auth0 
                                          (1- index-userinfo))
                                         (subseq
					  uri-string
					  index-userinfo
					  index-host)
                                         (parse-integer
					  (subseq 
                                           uri-string 
                                           (1+ index-host) 
                                           index-port))
                                         (subseq
					  uri-string
					  (1+ index-port)
                                          index-path)
                                         (subseq
					  uri-string
					  (1+ index-path)
                                          index-query)
                                         NIL)))
                                  (if (checkFragment
				       uri
				       0
				       index-path)
                                      (let
					  (
					   (index-fragment
					    (fragment
					     uri
					     0
					     (1+ index-path)
					     0)))     
                                        ;; ZOS USERINFO HOST
					;; PORT / PATH FRAGMENT
                                        (list
					 (subseq
					  uri-string
					  0 
                                          (1- index-scheme))
                                         (subseq
					  uri-string
					  index-auth0 
                                          (1- index-userinfo))
                                         (subseq
					  uri-string
					  index-userinfo
                                          index-host)
                                         (parse-integer
					  (subseq 
					   uri-string 
                                           (1+ index-host) 
                                           index-port))
                                         (subseq
					  uri-string
					  (1+ index-port)
                                          index-path)
                                         NIL
                                         (subseq
					  uri-string
					  (1+ index-path)
                                          index-fragment)))
                                    ;; ZOS USERINFO HOST PORT
				    ;; / PATH
                                    (list (subseq
					   uri-string
					   0 
                                           (1- index-scheme))
                                          (subseq
					   uri-string
					   index-auth0 
                                           (1- index-userinfo))
                                          (subseq
					   uri-string
					   index-userinfo
                                           index-host)
                                          (parse-integer
					   (subseq 
                                            uri-string 
                                            (1+ index-host) 
                                            index-port))
                                          (subseq
					   uri-string
					   (1+ index-port)
                                           index-path)
                                          NIL
                                          NIL))))
                            (if (checkQuery
				 uri
				 0
				 (1+
				  index-port))
                                (let ((index-query
				       (query
					uri 0
                                        (+ 2
					   index-port)
					0)))
                                  (if (checkFragment
				       uri
				       0
				       index-query)
                                      (let ((index-fragment
					     (fragment
					      uri
                                              0 
                                              (1+ index-query)
                                              0)))
                                        ;; ZOS USERINFO HOST PORT /
					;; QUERY FRAGMENT
                                        (list
					 (subseq
					  uri-string
					  0 
                                          (1- index-scheme))
                                         (subseq
					  uri-string
					  index-auth0 
                                          (1- index-userinfo))
                                         (subseq
					  uri-string
					  index-userinfo
                                          index-host)
					 (parse-integer
					  (subseq 
                                           uri-string 
                                           (1+ index-host) 
                                           index-port))
                                         NIL
                                         (subseq
					  uri-string
					  (+ 2 index-port)
                                          index-query)
                                         (subseq
					  uri-string
					  (1+ index-query)
                                          index-fragment)))
                                    ;; ZOS USERINFO
				    ;; HOST PORT /
				    ;; QUERY
                                    (list
				     (subseq
				      uri-string
				      0 
                                      (1-
				       index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     NIL
                                     (subseq
				      uri-string
				      (+ 2
					 index-port)
                                      index-query)
                                     NIL)))
                              (if (checkfragment
				   uri
				   0
				   (1+
				    index-port))
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (+ 2
					     index-port)
                                          0)))
                                    ;; ZOS USERINFO
				    ;; HOST PORT /
				    ;; FRAGMENT
                                    (list
				     (subseq
				      uri-string 0 
                                      (1- index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     NIL
                                     NIL
                                     (subseq
				      uri-string
				      (+ 2
					 index-port)
				      index-fragment)))
                                ;; ZOS USERINFO HOST
				;; PORT /
                                (if (checkEnd
				     uri
				     0
				     (1+
				      index-port))
                                    (list
				     (subseq
				      uri-string
				      0 
                                      (1-
				       index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     NIL
                                     NIL
                                     NIL)
                                  (uri-error)))))
                        ;; ZOS USERINFO HOST PORT
                        (if (checkEnd uri 0 index-port)
                            (list
			     (subseq
			      uri-string
			      0 
                              (1-
			       index-scheme))
                             (subseq
			      uri-string
			      index-auth0 
                              (1-
			       index-userinfo))
                             (subseq
			      uri-string
			      index-userinfo
                              index-host)
                             (parse-integer
			      (subseq 
                               uri-string 
                               (1+
				index-host) 
                               index-port))
                             NIL
                             NIL
                             NIL)
                          (uri-error))))
                  (if (checkSlash uri 0 index-host)
                      (if (checkPath
			   uri
			   0
			   (1+
			    index-host))
                          (let ((index-path
				 (path-zos
				  uri
				  0 
                                  (1+
				   index-host)
				  0 -1 0)))
                            (if (checkQuery
				 uri
				 0
				 index-path)
                                (let ((index-query
				       (query
					uri
					0
                                        (1+
					 index-path)
					0)))
                                  (if (checkFragment
				       uri
				       0
				       index-query)
                                      (let ((index-fragment
					     (fragment
					      uri
                                              0 
                                              (1+
					       index-query)
                                              0)))
                                        ;; ZOS USERINFO HOST /
					;; PATH QUERY FRAGMENT
                                        (list
					 (subseq
					  uri-string
					  0 
                                          (1-
					   index-scheme))
                                         (subseq
					  uri-string
					  index-auth0 
                                          (1-
					   index-userinfo))
                                         (subseq
					  uri-string
					  index-userinfo
                                          index-host)
                                         def-port
                                         (subseq
					  uri-string
					  (1+
					   index-host)
                                          index-path)
                                         (subseq
					  uri-string
					  (1+
					   index-path)
                                          index-query)
                                         (subseq
					  uri-string
					  (1+
					   index-query)
                                          index-fragment)))
                                    ;; ZOS USERINFO
				    ;; HOST / PATH
				    ;; QUERY
                                    (list
				     (subseq
				      uri-string 0 
                                      (1-
				       index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     def-port
                                     (subseq
				      uri-string
				      (1+
				       index-host)
                                      index-path)
                                     (subseq
				      uri-string
				      (1+
				       index-path)
                                      index-query)
                                     NIL)))
                              (if (checkfragment
				   uri
				   0
				   index-path)
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (1+
					   index-path)
                                          0)))
                                    ;; ZOS USERINFO
				    ;; HOST / PATH
				    ;; FRAGMENT
                                    (list
				     (subseq
				      uri-string 0 
                                      (1-
				       index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     def-port
                                     (subseq
				      uri-string
				      (1+
				       index-host)
                                      index-path)
                                     NIL
                                     (subseq
				      uri-string
				      (1+
				       index-path)
                                      index-fragment)))
                                ;; ZOS USERINFO HOST /
				;; PATH
                                (list
				 (subseq
				  uri-string 0 
                                  (1-
				   index-scheme))
                                 (subseq
				  uri-string
				  index-auth0 
                                  (1-
				   index-userinfo))
                                 (subseq
				  uri-string
				  index-userinfo
                                  index-host)
                                 def-port
                                 (subseq
				  uri-string
				  (1+
				   index-host)
                                  index-path)
                                 NIL
                                 NIL))))
                        (if (checkQuery
			     uri
			     0
			     (1+
			      index-host))
                            (let ((index-query
				   (query
				    uri
				    0
                                    (+ 2
				       index-host)
				    0)))
                              (if (checkFragment
				   uri
				   0
				   index-query)
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (1+
					   index-query)
                                          0)))
                                    ;; ZOS USERINFO
				    ;; HOST / QUERY
				    ;; FRAGMENT
                                    (list
				     (subseq
				      uri-string 0 
                                      (1-
				       index-scheme))
                                     (subseq
				      uri-string
				      index-auth0 
                                      (1-
				       index-userinfo))
                                     (subseq
				      uri-string
				      index-userinfo
                                      index-host)
                                     def-port
                                     NIL
                                     (subseq
				      uri-string
				      (+ 2
					 index-host)
                                      index-query)
                                     (subseq
				      uri-string
				      (1+
				       index-query)
                                      index-fragment)))
                                ;; ZOS USERINFO HOST
				;; / QUERY
                                (list
				 (subseq
				  uri-string 0 
                                  (1-
				   index-scheme))
                                 (subseq
				  uri-string
				  index-auth0 
                                  (1-
				   index-userinfo))
                                 (subseq
				  uri-string
				  index-userinfo
                                  index-host)
                                 def-port
                                 NIL
                                 (subseq
				  uri-string
				  (+ 2
				     index-host)
                                  index-query)
                                 NIL)))
                          (if (checkFragment
			       uri 0 (1+
				      index-host))
                              (let ((index-fragment
				     (fragment
				      uri
                                      0 
                                      (+ 2
					 index-host)
                                      0)))
                                ;; ZOS USERINFO HOST /
				;; FRAGMENT
                                (list
				 (subseq
				  uri-string
				  0 
                                  (1-
				   index-scheme))
                                 (subseq
				  uri-string
				  index-auth0 
                                  (1-
				   index-userinfo))
                                 (subseq
				  uri-string
				  index-userinfo
                                  index-host)
                                 def-port
                                 NIL
                                 NIL
                                 (subseq
				  uri-string
				  (+ 2 index-host)
				  index-fragment)))
                            ;; ZOS USERINFO HOST  /
                            (if (checkEnd
				 uri
				 0
				 (1+
				  index-host))
                                (list
				 (subseq
				  uri-string
				  0 
                                  (1-
				   index-scheme))
                                 (subseq
				  uri-string
				  index-auth0 
                                  (1-
				   index-userinfo))
                                 (subseq uri-string
					 index-userinfo
                                         index-host)
                                 def-port
                                 NIL
                                 NIL
                                 NIL)
                              (uri-error)))))
                    ;; ZOS USERINFO HOST
                    (if (checkEnd uri 0 index-host)
                        (list
			 (subseq
			  uri-string
			  0 
                          (1-
			   index-scheme))
                         (subseq
			  uri-string
			  index-auth0 
                          (1-
			   index-userinfo))
                         (subseq
			  uri-string
			  index-userinfo
                          index-host)
                         def-port
                         NIL
                         NIL
                         NIL)
                      (uri-error))))))
          (let ((index-host
		 (or
		  (host
		   uri
		   0
		   index-auth0
		   0)
                  (ip
		   uri
		   0
		   index-auth0
		   0
		   0
		   0))))
            (if (checkPort
		 uri 0 index-host)
                (let ((index-port
		       (port
			uri 0
			(1+
			 index-host)
			0)))
                  (if (checkSlash
		       uri
		       0
		       index-port)
                      (if (checkPath
			   uri
			   0
			   (1+
			    index-port))
                          (let ((index-path
				 (path-zos
				  uri
				  0 
                                  (1+
				   index-port)
				  0 -1 0)))
                            (if (checkQuery
				 uri
				 0
				 index-path)
                                (let ((index-query
				       (query
					uri
					0
                                        (1+
					 index-path)
					0)))
                                  (if (checkFragment
				       uri
				       0
				       index-query)
				      (let
					  (
					   (
					    index-fragment
					    (fragment
					     uri
                                             0 
                                             (1+
					      index-query)
                                             0)))
                                        ;; ZOS HOST PORT / PATH
					;; QUERY FRAGMENT
                                        (list
					 (subseq
					  uri-string
					  0 
                                          (1-
					   index-scheme))
                                         NIL
                                         (subseq
					  uri-string
					  index-auth0
                                          index-host)
                                         (parse-integer
					  (subseq 
                                           uri-string 
                                           (1+
					    index-host)
                                           index-port))
                                         (subseq
					  uri-string
					  (1+
					   index-port)
                                          index-path)
                                         (subseq
					  uri-string
					  (1+
					   index-path)
                                          index-query)
                                         (subseq
					  uri-string
					  (1+
					   index-query)
                                          index-fragment)
					 ))
                                    ;; ZOS HOST PORT
				    ;; / PATH QUERY
                                    (list
				     (subseq
				      uri-string 0 
                                      (1-
				       index-scheme))
                                     NIL
                                     (subseq
				      uri-string
				      index-auth0
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     (subseq
				      uri-string
				      (1+
				       index-port)
                                      index-path)
                                     (subseq
				      uri-string
				      (1+
				       index-path)
                                      index-query)
                                     NIL)))
                              (if (checkfragment
				   uri
				   0
				   index-path)
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (1+
					   index-path)
                                          0)))      
                                    ;; ZOS HOST PORT
				    ;; / PATH FRAGMENT
                                    (list
				     (subseq
				      uri-string
				      0 
				      (1-
				       index-scheme))
                                     NIL
                                     (subseq
				      uri-string
				      index-auth0
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     (subseq
				      uri-string
				      (1+
				       index-port)
                                      index-path)
                                     NIL
                                     (subseq
				      uri-string
				      (1+
				       index-path)
                                      index-fragment)))
                                ;; ZOS HOST PORT /
				;; PATH
                                (list
				 (subseq
				  uri-string 0 
                                  (1-
				   index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 (parse-integer
				  (subseq 
                                   uri-string 
                                   (1+
				    index-host) 
                                   index-port))
                                 (subseq
				  uri-string
				  (1+
				   index-port)
                                  index-path)
                                 NIL
                                 NIL))))
                        (if (checkQuery
			     uri
			     0
			     (1+
			      index-port))
                            (let ((index-query
				   (query
				    uri
				    0
                                    (+ 2
				       index-port)
				    0)))
                              (if (checkFragment
				   uri
				   0
				   index-query)
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (1+
					   index-query)
                                          0)))
                                    ;; ZOS HOST PORT
				    ;; / QUERY FRAGMENT
                                    (list
				     (subseq
				      uri-string
				      0 
                                      (1-
				       index-scheme))
                                     NIL
                                     (subseq
				      uri-string
				      index-auth0
                                      index-host)
                                     (parse-integer
				      (subseq 
                                       uri-string 
                                       (1+
					index-host) 
                                       index-port))
                                     NIL
                                     (subseq
				      uri-string
				      (+ 2
					 index-port)
                                      index-query)
                                     (subseq
				      uri-string
				      (1+
				       index-query)
                                      index-fragment)))
                                ;; ZOS HOST PORT /
				;; QUERY
                                (list
				 (subseq
				  uri-string
				  0 
                                  (1-
				   index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 (parse-integer
				  (subseq 
                                   uri-string 
                                   (1+
				    index-host) 
                                   index-port))
                                 NIL
                                 (subseq
				  uri-string
				  (+ 2
				     index-port)
                                  index-query)
                                 NIL)))
                          (if (checkfragment
			       uri
			       0
			       (1+
				index-port))
                              (let ((index-fragment
				     (fragment
				      uri
                                      0 
                                      (+ 2
					 index-port)
                                      0)))
                                ;; ZOS HOST PORT /
				;; FRAGMENT
                                (list
				 (subseq
				  uri-string
				  0 
                                  (1-
				   index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 (parse-integer
				  (subseq 
                                   uri-string 
                                   (1+
				    index-host) 
                                   index-port))
                                 NIL
                                 NIL
                                 (subseq
				  uri-string
				  (+ 2
				     index-port)
				  index-fragment)))
                            ;; ZOS HOST PORT /
                            (if (checkEnd
				 uri
				 0
				 (1+
				  index-port))
                                (list
				 (subseq
				  uri-string
				  0 
                                  (1-
				   index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 (parse-integer
				  (subseq 
                                   uri-string 
                                   (1+ index-host) 
                                   index-port))
                                 NIL
                                 NIL
                                 NIL)
                              (uri-error)))))
                    ;; ZOS HOST PORT
                    (if (checkEnd
			 uri
			 0
			 index-port)
                        (list
			 (subseq
			  uri-string 0 
                          (1-
			   index-scheme))
                         NIL
                         (subseq
			  uri-string
			  index-auth0
                          index-host)
                         (parse-integer
			  (subseq 
                           uri-string 
                           (1+ index-host) 
                           index-port))
                         NIL
                         NIL
                         NIL)
                      (uri-error))))
              (if (checkSlash uri 0 index-host)
                  (if (checkPath uri 0 (1+ index-host))
                      (let ((index-path 
			     (path-zos uri 0 
                                       (1+
					index-host)
				       0 -1 0)))
                        (if (checkQuery
			     uri 0 index-path)
                            (let ((index-query
				   (query
				    uri
				    0
                                    (1+
				     index-path)
				    0)))
                              (if (checkFragment
				   uri
				   0
				   index-query)
                                  (let ((index-fragment
					 (fragment
					  uri
                                          0 
                                          (1+
					   index-query)
                                          0)))
                                    ;; ZOS HOST / PATH
				    ;; QUERY FRAGMENT
                                    (list
				     (subseq
				      uri-string 0 
                                      (1- index-scheme))
                                     NIL
                                     (subseq
				      uri-string
				      index-auth0
                                      index-host)
                                     def-port
                                     (subseq
				      uri-string
				      (1+
				       index-host)
                                      index-path)
                                     (subseq
				      uri-string
				      (1+
				       index-path)
                                      index-query)
                                     (subseq
				      uri-string
				      (1+
				       index-query)
                                      index-fragment)))
                                ;; ZOS HOST / PATH
				;; QUERY
                                (list
				 (subseq
				  uri-string 0 
                                  (1-
				   index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 def-port
                                 (subseq
				  uri-string
				  (1+
				   index-host)
                                  index-path)
                                 (subseq
				  uri-string
				  (1+
				   index-path)
                                  index-query)
                                 NIL)))
                          (if (checkfragment
			       uri 0 index-path)
                              (let ((index-fragment
				     (fragment
				      uri
                                      0 
                                      (1+
				       index-path)
                                      0)))
                                ;; ZOS HOST / PATH
				;; FRAGMENT
                                (list
				 (subseq
				  uri-string 0 
                                  (1- index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 def-port
                                 (subseq
				  uri-string
				  (1+
				   index-host)
                                  index-path)
                                 NIL
                                 (subseq
				  uri-string
				  (1+
				   index-path)
                                  index-fragment)))
                            ;; ZOS HOST / PATH
                            (list
			     (subseq
			      uri-string
			      0 
                              (1-
			       index-scheme))
                             NIL
                             (subseq
			      uri-string
			      index-auth0
                              index-host)
                             def-port
                             (subseq
			      uri-string
			      (1+
			       index-host)
                              index-path)
                             NIL
                             NIL))))
                    (if (checkQuery
			 uri
			 0
			 (1+
			  index-host))
                        (let ((index-query
			       (query
				uri
				0
                                (+ 2
				   index-host)
				0)))
                          (if (checkFragment
			       uri
			       0
			       index-query)
                              (let ((index-fragment
				     (fragment
				      uri
                                      0 
                                      (1+
				       index-query)
                                      0)))
                                ;; ZOS HOST / QUERY
				;; FRAGMENT
                                (list
				 (subseq
				  uri-string 0 
                                  (1- index-scheme))
                                 NIL
                                 (subseq
				  uri-string
				  index-auth0
                                  index-host)
                                 def-port
                                 NIL
                                 (subseq
				  uri-string
				  (+ 2
				     index-host)
                                  index-query)
                                 (subseq
				  uri-string
				  (1+
				   index-query)
                                  index-fragment)))
                            ;; ZOS HOST / QUERY
                            (list
			     (subseq
			      uri-string 0 
                              (1- index-scheme))
                             NIL
                             (subseq
			      uri-string index-auth0
                              index-host)
                             def-port
                             NIL
                             (subseq
			      uri-string
			      (+ 2
				 index-host)
                              index-query)
                             NIL)))
                      (if (checkfragment
			   uri 0 (1+ index-host))
                          (let ((index-fragment
				 (fragment
				  uri
                                  0 
                                  (+ 2
				     index-host)
                                  0)))
                            ;; ZOS HOST / FRAGMENT
                            (list (subseq
				   uri-string 0 
                                   (1- index-scheme))
                                  NIL
                                  (subseq
				   uri-string
				   index-auth0
                                   index-host)
                                  def-port
                                  NIL
                                  NIL
                                  (subseq
				   uri-string
				   (+ 2
				      index-host)
				   index-fragment)))
                        ;; ZOS HOST  /
                        (if (checkEnd
			     uri
			     0 (1+
				index-host))
                            (list
			     (subseq
			      uri-string
			      0 
                              (1-
			       index-scheme))
                             NIL
                             (subseq
			      uri-string
			      index-auth0
                              index-host)
                             def-port
                             NIL
                             NIL
                             NIL)
                          (uri-error)))))
                ;; ZOS HOST
                (if (checkend uri 0 index-host)
                    (list (subseq
			   uri-string
			   0 
                           (1- index-scheme))
                          NIL
                          (subseq
			   uri-string
			   index-auth0
                           index-host)
                          def-port
                          NIL
                          NIL
                          NIL)
                  (uri-error)))))))
    (if (checkSlash uri 0 index-scheme)
        (if (checkPath uri 0 (1+ index-scheme))
            (let ((index-path
		   (path-zos
		    uri
		    0 
                    (1+ index-scheme)
		    0
		    -1
		    0)))
              (if (checkQuery uri 0 index-path)
                  (let ((index-query
			 (query
			  uri
			  0
                          (1+ index-path)
			  0)))
                    (if (checkFragment
			 uri 0 index-query)
                        (let ((index-fragment
			       (fragment
				uri
                                0 
                                (1+ index-query)
                                0)))
                          ;; ZOS / PATH QUERY FRAGMENT
                          (list
			   (subseq
			    uri-string
			    0 
                            (1- index-scheme))
                           NIL
                           NIL
                           def-port
                           (subseq
			    uri-string
			    (1+ index-scheme)
                            index-path)
                           (subseq
			    uri-string
			    (1+ index-path)
                            index-query)
                           (subseq
			    uri-string
			    (1+ index-query)
                            index-fragment)))
                      ;; ZOS / PATH QUERY
                      (list
		       (subseq
			uri-string
			0 
                        (1- index-scheme))
                       NIL
                       NIL
                       def-port
                       (subseq
			uri-string
			(1+ index-scheme)
                        index-path)
                       (subseq
			uri-string
			(1+ index-path)
                        index-query)
                       NIL)))
                (if (checkfragment
		     uri 0 index-path)
                    (let ((index-fragment
			   (fragment
			    uri
                            0 
                            (1+ index-path)
                            0)))                       
                      ;; ZOS / PATH FRAGMENT
                      (list
		       (subseq
			uri-string
			0 
                        (1- index-scheme))
                       NIL
                       NIL
                       def-port
                       (subseq
			uri-string
			(1+ index-scheme)
                        index-path)
                       NIL
                       (subseq
			uri-string
			(1+ index-path)
                        index-fragment)))
                  ;; ZOS / PATH
                  (list (subseq
			 uri-string 0 
                         (1- index-scheme))
                        NIL
                        NIL
                        def-port
                        (subseq
			 uri-string (1+ index-scheme)
                         index-path)
                        NIL
                        NIL))))
          (if (checkQuery uri 0 (1+ index-scheme))
              (let ((index-query
		     (query
		      uri
		      0
                      (+ 2 index-scheme)
		      0)))
                (if (checkFragment uri 0 index-query)
                    (let ((index-fragment
			   (fragment
			    uri
                            0 
                            (1+ index-query)
                            0)))
                      ;; ZOS / QUERY FRAGMENT
                      (list
		       (subseq
			uri-string 0 
                        (1- index-scheme))
                       NIL
                       NIL
                       def-port
                       NIL
                       (subseq
			uri-string
			(+ 2 index-scheme)
                        index-query)
                       (subseq
			uri-string
			(1+ index-query)
                        index-fragment)))
                  ;; ZOS / QUERY
                  (list
		   (subseq
		    uri-string 0 
                    (1- index-scheme))
                   NIL
                   NIL
                   def-port
                   NIL
                   (subseq
		    uri-string
		    (+ 2 index-scheme)
                    index-query)
                   NIL)))
            (if (checkfragment uri 0 (1+ index-scheme))
                (let ((index-fragment
		       (fragment
			uri
                        0 
                        (+ 2 index-scheme)
                        0)))
                  ;; ZOS / FRAGMENT
                  (list
		   (subseq
		    uri-string
		    0 
                    (1- index-scheme))
                   NIL
                   NIL
                   def-port
                   NIL
                   NIL
                   (subseq
		    uri-string
		    (+ 2
		       index-scheme) 
                    index-fragment)))
              ;; ZOS /
              (if (checkEnd uri 0 (1+ index-scheme))
                  (list
		   (subseq
		    uri-string
		    0 
                    (1- index-scheme))
                   NIL
                   NIL
                   def-port
                   NIL
                   NIL
                   NIL)
                (uri-error)))))
      ;; ZOS
      (if (checkEnd uri 0 index-scheme)
          (list
	   (subseq
	    uri-string
	    0 
            (1- index-scheme))
           NIL
           NIL
           def-port
           NIL
           NIL
           NIL)))))

;;; Generic URI error: URI provided is not valid.
(defun uri-error ()
  (error "URI is not valid!"))

;;;; end of file -- uri-parse.lisp
