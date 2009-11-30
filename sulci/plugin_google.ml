(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *     2009      sh!zeeg port to new json API <sh!zeeg@ya.ru>
 *)
open Xml
open Xmpp
open Types
open Hooks
open Common
open Http_suck
open Printf

type json search_results =
     <  response_data "responseData":
      < results "results": item list > >

and item = 
    <   gsearch_result_class "GsearchResultClass": string;
        unescaped_url "unescapedUrl": string;
        url "url": string;
        visible_url "visibleUrl": string;
        cache_url "cacheUrl": string;
        title "title": string;
        title_no_formatting "titleNoFormatting": string;
        content "content": string >

and cursor = 
    <  cursor "cursor":
     <  estimated_result_count "estimatedResultCount": int;
        current_page_index "currentPageIndex": int;
        more_results_url "moreResultsUrl": string;
        pages "pages": page list > >

and page = 
    <   start "start": int;
        label "label": int >

let fetch query = 
  let j = (Json_io.json_of_string query) in
  search_results_of_json j

let google ?(start="0") ?(items="1") text from xml env out =
  if text = "" then
    make_msg out xml (Lang.get_msg env.env_lang "plugin_google_bad_syntax" [])
  else
    let callback data = 
      let response_tail = ref None in
      let resp = match data with
        | OK (_media, _charset, content) -> (
            try
	      let parsed = fetch content in
	      let res = parsed#response_data in
	      let x = List.nth res#results (int_of_string start) in
  	      sprintf "%s\n%s\n%s" (Dehtml.html2text x#title) (Dehtml.html2text x#content) (Dehtml.html2text x#url)  
	    
	    (* with Not_found ->
              Lang.get_msg env.env_lang "plugin_google_not_found" []; *)
	    with Failure start ->
	      Lang.get_msg env.env_lang "plugin_google_not_found" []
          )
        | Exception exn ->
            match exn with
              | ClientError ->
                  Lang.get_msg env.env_lang "plugin_google_404" []
              | ServerError ->
                  Lang.get_msg env.env_lang "plugin_google_server_error" []
	      | Failure start ->
		  Lang.get_msg env.env_lang "plugin_google_not_found" []
              | _ ->
                  Lang.get_msg env.env_lang "plugin_google_server_error" []
      in
        make_msg out xml ?response_tail:!response_tail resp 
    in
    let url = 
      "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ^
        Netencoding.Url.encode (text) in
      Http_suck.http_get url callback

let rx = Pcre.regexp "([0-9]+) (.+)"

let google_adv text from xml env out =
  try
    let r = Pcre.exec ~rex:rx text in
    let start = Pcre.get_substring r 1 in
    let items = "1" in
    let request = Pcre.get_substring r 2 in
      google ~start ~items request from xml env out
  with Not_found -> 
    make_msg out xml (Lang.get_msg env.env_lang
                        "plugin_google_adv_invalid_syntax" [])

let _ =
  register_command "google" google;
  register_command "google_adv" google_adv
