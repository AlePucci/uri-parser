# LP_E1P_2022: AN URI STRING PARSER

Group members:
- Alessandro Pucci 869177
- Matias Aldo Ruiz 869139

The purpose of this project is to create a Common-Lisp library that can build 
structures, internally representing URIs (Uniform Resource Identifiers), 
starting from their representations as strings.
Parsing requires analyzing and decomposing the input string in sequence, 
character from character from left to right, in order to build a proper 
structure that stores its seven components.

An URI is made of "Scheme", "Userinfo, "Host", "Port", "Path", "Query" and
"Fragment".

An uri-structure is like this: uri(Scheme, Userinfo, Host, Port, Path,
Query, Fragment).

If not specified, in every scheme, 80 is the default port.
An empty field, if the URI is still valid, is set to NIL.

If uri-parse is called with an invalid URI-STRING, it returns the following
error: "URI is not valid!".

If uri-parse is called with a non-string object, it returns the following
error: "URI is not a string!".

-----------------------------------------------------------------------------

Call (uri-parse URI-STRING) in order to get URI structure.

Call (uri-scheme (uri-parse URI-STRING)) in order to get scheme part of URI. 

Call (uri-userinfo (uri-parse URI-STRING)) in order to get userinfo part of
     URI if exists, NIL otherwise.
     
Call (uri-host (uri-parse URI-STRING)) in order to get host part of
     URI if exists, NIL otherwise.
     
Call (uri-port (uri-parse URI-STRING)) in order to get port part of
     URI if specified (as an Integer), 80 otherwise.
     
Call (uri-path (uri-parse URI-STRING)) in order to get path part of
     URI if exists, NIL otherwise.
     
Call (uri-query (uri-parse URI-STRING)) in order to get query part of
     URI if exists, NIL otherwise.
     
Call (uri-fragment (uri-parse URI-STRING)) in order to get fragment part of
     URI if exists, NIL otherwise.
     
Call (uri-display (uri-parse URI-STRING)) in order to print on screen
     (std output) URI fields.
     
Note that *uri-display automatically closes the stream!*
     
Call (uri-display (uri-parse URI-STRING) OutputStream) in order to print
     on the provided OutputStream URI fields.
     
Note that *uri-display automatically closes the stream!*

Thanks for reading!
