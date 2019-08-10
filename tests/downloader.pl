% Usage:
% $ swipl -f none -g ensure_clex -t halt -s downloader.pl
% $ swipl -f none -g download_acetexts -t halt -s downloader.pl

clex_remote_local('https://raw.github.com/Attempto/Clex/master/clex_lexicon.pl', 'clex_lexicon.pl').
acetexts_remote_local('http://attempto.ifi.uzh.ch/cgi-bin/acetextset/get_acetexts.cgi', 'acetexts.pl').

%!  ensure_clex
%
%   Download the CLex lexicon if not already present.

ensure_clex :-
    clex_remote_local(Url, Filename),
    (exists_file(Filename) -> true ; download(Url, Filename)).

%!  download_acetexts
%
%   Download the APE regression testsuite.

download_acetexts :-
    acetexts_remote_local(Url, Filename),
    download(Url, Filename).

download(Url, Filename) :-
    format(user_error, "Saving ~w as ~w~n", [Url, Filename]),
    use_module(library(http/http_open)),
    setup_call_cleanup(
        (
            http_open(Url, In, []),
            set_stream(In, encoding(utf8))
        ),
        setup_call_cleanup(
            open(Filename, write, Out),
            copy_stream_data(In, Out),
            close(Out)
        ),
        close(In)
    ).
