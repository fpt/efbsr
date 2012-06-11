%% @author Youichi FUJIMOTO <yofujimo@gmail.com>
%% @copyright 2012 Youichi Fujimoto, All rights reserved. Open source, BSD License.
%% @doc efbsr_auth: Facebook Signed Request authentication module


-module(efbsr).
-export([
    parse/1,
    parse_signed_request/1,
    get_app_id/0,
    get_app_secret/0
]).

parse(SignedReq) ->
    Req = parse_signed_request(SignedReq),
    Code = proplists:get_value("code", Req),
    parse_get_oauth_token(Req, Code).


parse_get_oauth_token(Req, undefined) ->
    proplists:get_value("oauth_token", Req);
parse_get_oauth_token(Req, Code) ->
    {ok, AppId} = application:get_env(efbsr, app_id),
    {ok, AppSecret} = application:get_env(efbsr, app_secret),
    Resp = fb_oauth_access_token(AppId, AppSecret, Code),
    erlang:display("Resp"),
    erlang:display(Resp),
    proplists:get_value("access_token", Resp).


parse_signed_request(SignedReq) ->
    {ok, AppSecret} = application:get_env(efbsr, app_secret),

    [Sig, B64Json] = string:tokens(SignedReq, "."),
    % decode json
    Json = binary_to_list(fb_decode_base64(B64Json)),
    % calc expected signature
    ExpSigStr = base64:encode_to_string(hmac:hmac256(AppSecret, B64Json)),
    Conv = jsonconv:conv(mochijson2:decode(Json)),
    % check algorithm
    Algo = string:to_upper(proplists:get_value("algorithm", Conv)),
    case Algo of
        "HMAC-SHA256" ->
            parse_signed_request_sha256(Sig, ExpSigStr, Conv);
        _ ->
            error
    end.

parse_signed_request_sha256(Sig, ExpSigStr, Conv) ->
    ExpSig = re:replace(
                re:replace(
                    re:replace(ExpSigStr, "\\+", "-", [{return, list}]),
                    "/", "_", [{return, list}]),
                "=", "", [{return, list}]),
    if
        Sig == ExpSig -> Conv;
        true -> error
    end.

get_app_id() ->
    {ok, AppId} = application:get_env(efbsr, app_id),
    AppId.

get_app_secret() ->
    {ok, AppId} = application:get_env(efbsr, app_secret),
    AppId.

% private

fb_oauth_access_token(AppId, Secret, Code) ->
    GraphUrl = "https://graph.facebook.com/oauth/access_token?client_id=" ++ AppId ++ "&client_secret=" ++ Secret ++ "&redirect_uri=&code=" ++ Code,
    {ok, {{_, 200, _}, _, RespBody}} = httpc:request(get, {GraphUrl, []}, [], []),
    mochiweb_util:parse_qs(RespBody).


% try adding trailing '='s to decode base64 string
% check
% http://erlang.org/pipermail/erlang-questions/2011-December/063487.html
fb_decode_base64(Base64) when is_list(Base64) ->
    try base64:decode(Base64)
    catch
        error:_ -> % could be missing =
            try base64:decode(Base64 ++ "=")
            catch
                error:_ -> % could be missing ==
                    try base64:decode(Base64 ++ "==")
                    catch
                        error:_ -> % base64 is really wrong. we cannot fix it
                            error
                    end
            end
    end.
