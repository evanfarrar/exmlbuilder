-module(xml).
-export([build/1, test/0]).

test() ->
  "<hey>there</hey>" = build({hey,there}),
  "<tag foo=\"bar\">content</tag>" = build({tag,[{foo,bar}],content}),
  "<tag><tag>content</tag></tag>" = build({tag,{tag,content}}),
  "<tag foo=\"bar\"><tag foo=\"bar\">content</tag></tag>" = build({tag,[{foo,bar}],{tag,[{foo,bar}],content}}),
  "<tag foo=\"bar\" foo=\"bar\">content</tag>" = build({tag,[{foo,bar},{foo,bar}],content}),
% this is not supported, it is ambiguous with the next test. ok?
%  "<tag foo=\"bar\"></tag>" = build({tag,[{foo,bar}]}),
  "<branch><leaf>a</leaf><leaf>b</leaf></branch>" = build({branch,[{leaf,a},{leaf,b}]}),
  "<branch foo=\"bar\"><leaf>a</leaf><leaf>b</leaf></branch>" = build({branch,
    [{foo,bar}],[{leaf,a},{leaf,b}]}),
  "<iq type=\"set\" id=\"auth\"><query xmlns=\"jabber:iq:auth\"><username>e"++
    "</username><password>e</password><resource>TelnetClient</resource>"++
    "</query></iq>" = build(
      {iq,[{type,set},{id,auth}],
        {'query',[{xmlns,'jabber:iq:auth'}],
          [{username,e},
           {password,e},
           {resource,'TelnetClient'}]
        }
      }),
  horray.



% this is not supported, it is ambiguous with the next test. ok?
%build({Tag,AttrList}) when is_list(AttrList) ->
%  build({Tag,AttrList,''});
build({Tag,Value}) ->
  build({Tag,[],Value});
build({Tag,AttrList,ContentList}) when is_list(ContentList) ->
  Content = list_to_atom(lists:flatmap(fun build/1, ContentList)),
  build({Tag,AttrList,Content});
build({Tag,AttrList,Content}) when is_tuple(Content) ->
%TODO: tail recur?
  build({Tag,AttrList,list_to_atom(build(Content))});
build({TAG,AttrList,Content}) ->
  Tag = atom_to_list(TAG),
  "<"++Tag++attrs(AttrList,"")++">"++atom_to_list(Content)++"</"++Tag++">".

attrs([],Acc) ->
  Acc;
attrs([H|T],Acc) ->
  {Attr,Val} = H,
  AttrStr = atom_to_list(Attr),
  ValStr = atom_to_list(Val),
  attrs(T,Acc ++" "++AttrStr++"=\""++ValStr++"\"").
