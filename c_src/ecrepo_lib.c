#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rpm/rpmlib.h>
#include <rpm/rpmts.h>

#include "erl_nif.h"

#define NOMEMORY    "nomemory"

    static ERL_NIF_TERM ecrepo_lib_header(ErlNifEnv *, int, const ERL_NIF_TERM []);
    static ERL_NIF_TERM ecrepo_lib_compare(ErlNifEnv *, int, const ERL_NIF_TERM []);
    static ERL_NIF_TERM ecrepo_lib_tag2name(ErlNifEnv *, int, const ERL_NIF_TERM []);
    static ERL_NIF_TERM ecrepo_lib_name2tag(ErlNifEnv *, int, const ERL_NIF_TERM []);
    static ERL_NIF_TERM ecrepo_lib_quote(ErlNifEnv *, int, const ERL_NIF_TERM []);

/*
 {{{ Forward declaration of helpers
 */
    static ERL_NIF_TERM _ecrepo_lib_header(ErlNifEnv *, FD_t, const char *, int *, int);
    static ERL_NIF_TERM _ecrepo_lib_header_all(ErlNifEnv *, Header);
    static ERL_NIF_TERM _ecrepo_lib_header_given(ErlNifEnv *, Header, int *, int);
    static ERL_NIF_TERM _ecrepo_lib_convert(ErlNifEnv *, rpmtd);
    static ERL_NIF_TERM _ecrepo_lib_convert_data(ErlNifEnv *, rpmtd, rpmTagClass);
    static ERL_NIF_TERM _ecrepo_lib_error(ErlNifEnv *, const char *);
    static ERL_NIF_TERM _ecrepo_lib_ok(ErlNifEnv *, ERL_NIF_TERM);
    static int *_list_to_list(ErlNifEnv *, ERL_NIF_TERM, int, ERL_NIF_TERM *);
    static ERL_NIF_TERM _string_to_binary(ErlNifEnv *, const char *);
    static ERL_NIF_TERM _binary_to_binary(ErlNifEnv *, const void *, size_t);
/*
 }}}
 */

static ErlNifFunc nif_funcs[] = {
    {"header", 1, ecrepo_lib_header},
    {"compare", 2, ecrepo_lib_compare},
    {"tag2name", 1, ecrepo_lib_tag2name},
    {"name2tag", 1, ecrepo_lib_name2tag},
    {"quote", 1, ecrepo_lib_quote}
};

static int DEFAULT_TAGS[] = {
    1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007,
    1009, 1011, 1014, 1015, 1016, 1020, 1022, 1030,
    1037, 1044, 1046, 1047, 1048, 1049, 1050, 1053,
    1054, 1055, 1080, 1081, 1082, 1090, 1106, 1112,
    1113, 1114, 1115, 1116, 1117, 1118
};

#define DEFAULT_TAGS_NO (sizeof(DEFAULT_TAGS)/sizeof(DEFAULT_TAGS[0]))

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    rpmts ts;

    rpmReadConfigFiles(NULL, NULL);

    ts = rpmtsCreate();
    rpmtsSetVSFlags(ts, _RPMVSF_NOSIGNATURES);

    *priv_data = ts;

    return 0;
}

ERL_NIF_INIT(ecrepo_lib, nif_funcs, &on_load, NULL, NULL, NULL);

static ERL_NIF_TERM ecrepo_lib_header(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary binary;
    char *filename;
    FD_t fd;
    ERL_NIF_TERM result;
    int *tags = NULL;
    int tags_no;

    if (argc < 1 || argc > 2 || !enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
        return enif_make_badarg(env);
    }

    if (argc == 2) {
        unsigned length;

        if (enif_get_atom_length(env, argv[1], &length, ERL_NIF_LATIN1)) {
            char *atom;

            if ((atom = (char *)calloc(length + 1, sizeof(char))) == NULL) {
                return _ecrepo_lib_error(env, NOMEMORY);
            } else {
                enif_get_atom(env, argv[1], atom, length, ERL_NIF_LATIN1);

                if (strcmp(atom, "all") == 0) {
                    tags = NULL;    /* Duplicate of above? */
                } else if (strcmp(atom, "index") == 0) {
                    tags = DEFAULT_TAGS;
                    tags_no = DEFAULT_TAGS_NO;
                } else {
                    free(atom);

                    return enif_make_badarg(env);
                }

                free(atom);
            }
        } else if (enif_get_list_length(env, argv[1], &length)) {
            if ((tags = _list_to_list(env, argv[1], length, &result)) == NULL) {
                return result;
            }
            tags_no = length;
        } else {
            return enif_make_badarg(env);
        }
    } else {
        tags = DEFAULT_TAGS;
        tags_no = DEFAULT_TAGS_NO;
    }

    if ((filename = (char *)calloc(binary.size + 1, sizeof(char))) == NULL) {
        return _ecrepo_lib_error(env, NOMEMORY);
    }

    memcpy(filename, binary.data, binary.size);

    if ((fd = Fopen(filename, "r")) == NULL) {
        free(filename);

        return _ecrepo_lib_error(env, "open");
    }

    result = _ecrepo_lib_header(env, fd, filename, tags, tags_no);

    if (tags != NULL && tags != DEFAULT_TAGS) {
        free(tags);
    }

    free(filename);
    Fclose(fd);

    return result;
}

static ERL_NIF_TERM ecrepo_lib_compare(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary first, second;
    char *first_string, *second_string;
    int result;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &first) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &second)) {
        return enif_make_badarg(env);
    }

    if ((first_string = (char *)calloc(first.size + 1, sizeof(char))) == NULL) {
        return _ecrepo_lib_error(env, NOMEMORY);
    }

    if ((second_string = (char *)calloc(second.size + 1, sizeof(char))) == NULL) {
        free(first_string);

        return _ecrepo_lib_error(env, NOMEMORY);
    }

    memcpy(first_string, first.data, first.size);
    memcpy(second_string, second.data, second.size);

    result = rpmvercmp(first_string, second_string);

    free(second_string);
    free(first_string);

    return enif_make_int(env, result);
}

static ERL_NIF_TERM ecrepo_lib_tag2name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int tag;

    if (!enif_get_int(env, argv[0], &tag)) {
        return enif_make_badarg(env);
    }

    const char *name = rpmTagGetName(tag);

    if (name != NULL) {
        return enif_make_atom(env, name);
    } else {
        return _ecrepo_lib_error(env, "unknown");
    }
}

static ERL_NIF_TERM ecrepo_lib_name2tag(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *name;

    if (enif_is_atom(env, argv[0])) {
        unsigned size;

        enif_get_atom_length(env, argv[0], &size, ERL_NIF_LATIN1);

        if ((name = calloc(size + 1, sizeof(char))) == NULL) {
            return _ecrepo_lib_error(env, NOMEMORY);
        }

        enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
    } else {
        ErlNifBinary binary;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
            return enif_make_badarg(env);
        }

        if ((name = (char *)calloc(binary.size + 1, sizeof(char))) == NULL) {
            return _ecrepo_lib_error(env, NOMEMORY);
        }

        memcpy(name, binary.data, binary.size);
    }

    ERL_NIF_TERM result = _ecrepo_lib_ok(env, enif_make_int(env, rpmTagGetValue(name)));

    free(name);

    return result;
}

static ERL_NIF_TERM ecrepo_lib_quote(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary binary, result;
    size_t dst, i;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
        return enif_make_badarg(env);
    }

    for (dst = i = 0; i < binary.size; i++) {
        switch (binary.data[i]) {
            case '&':
                dst += 5;
                break;

            case '<':
                dst += 4;
                break;

            case '>':
                dst += 4;
                break;

            case '"':
                dst += 6;
                break;

            default:
                ++dst;
        }
    }

    if (dst == binary.size) {
        return argv[0];
    }

    if (!enif_alloc_binary(dst, &result)) {
        return _ecrepo_lib_error(env, NOMEMORY);
    }

    for (dst = i = 0; i < binary.size; i++) {
        switch (binary.data[i]) {
            case '&':
                result.data[dst++] = '&';
                result.data[dst++] = 'a';
                result.data[dst++] = 'm';
                result.data[dst++] = 'p';
                result.data[dst++] = ';';
                break;

            case '<':
                result.data[dst++] = '&';
                result.data[dst++] = 'l';
                result.data[dst++] = 't';
                result.data[dst++] = ';';
                break;

            case '>':
                result.data[dst++] = '&';
                result.data[dst++] = 'g';
                result.data[dst++] = 't';
                result.data[dst++] = ';';
                break;

            case '"':
                result.data[dst++] = '&';
                result.data[dst++] = 'q';
                result.data[dst++] = 'u';
                result.data[dst++] = 'o';
                result.data[dst++] = 't';
                result.data[dst++] = ';';
                break;

            default:
                result.data[dst++] = binary.data[i];
        }
    }

    return enif_make_binary(env, &result);
}

/*
{{{ Helpers
 */
static ERL_NIF_TERM _ecrepo_lib_header(ErlNifEnv *env, FD_t fd, const char *filename, int *tags, int tags_no) {
    Header h;
    rpmRC rc;
    rpmts ts = enif_priv_data(env);

    if ((rc = rpmReadPackageFile(ts, fd, filename, &h)) != RPMRC_OK) {
        return _ecrepo_lib_error(env, "read_package");
    }

    return tags == NULL ? _ecrepo_lib_header_all(env, h) :
                          _ecrepo_lib_header_given(env, h, tags, tags_no);
}

static ERL_NIF_TERM _ecrepo_lib_header_all(ErlNifEnv *env, Header h) {
    HeaderIterator hi;
    struct rpmtd_s tag_data;
    ERL_NIF_TERM result = enif_make_list(env, 0);

    for (hi = headerInitIterator(h); headerNext(hi, &tag_data); rpmtdFreeData(&tag_data)) {
        if (tag_data.tag < 1000) {
            continue;
        }

        result = enif_make_list_cell(env,
                                     enif_make_tuple2(env,
                                                      enif_make_int(env, tag_data.tag),
                                                      _ecrepo_lib_convert(env, &tag_data)),
                                     result);
    }

    headerFreeIterator(hi);

    return _ecrepo_lib_ok(env, result);
}

static ERL_NIF_TERM _ecrepo_lib_header_given(ErlNifEnv *env, Header h, int *tags, int tags_no) {
    struct rpmtd_s tag_data;
    ERL_NIF_TERM result = enif_make_list(env, 0);
    int i;

    for (i = 0; i < tags_no; ++i) {
        if (!headerGet(h, tags[i], &tag_data, HEADERGET_DEFAULT)) {
            continue;       /* Skip missing tags */
        }

        result = enif_make_list_cell(env,
                                     enif_make_tuple2(env,
                                                      enif_make_int(env, tag_data.tag),
                                                      _ecrepo_lib_convert(env, &tag_data)),
                                     result);

        rpmtdFreeData(&tag_data);
    }

    return _ecrepo_lib_ok(env, result);
}

static ERL_NIF_TERM _ecrepo_lib_convert(ErlNifEnv *env, rpmtd tag_data) {
    ERL_NIF_TERM result;

    rpmTagClass klass = rpmTagTypeGetClass(tag_data->type);

    if (rpmTagGetReturnType(tag_data->tag) == RPM_ARRAY_RETURN_TYPE) {
        /* The order of items should be kept and it seems unreasonable to
            * create and reverse a list
            */
        ERL_NIF_TERM *tempo = (ERL_NIF_TERM *)calloc(rpmtdCount(tag_data), sizeof(ERL_NIF_TERM));

        if (tempo == NULL) {
            result = _ecrepo_lib_error(env, NOMEMORY);
        } else {
            int i;

            for (i = 0; rpmtdNext(tag_data) >= 0; ++i) {
                tempo[i] = _ecrepo_lib_convert_data(env, tag_data, klass);
            }

            result = enif_make_list_from_array(env, tempo, i);

            free(tempo);
        }
    } else {
        result = _ecrepo_lib_convert_data(env, tag_data, klass);
    }

    return result;
}

static ERL_NIF_TERM _ecrepo_lib_convert_data(ErlNifEnv *env, rpmtd tag_data, rpmTagClass klass) {
    switch (klass) {
        case RPM_NULL_CLASS:
            return enif_make_atom(env, "null");

        case RPM_NUMERIC_CLASS:
            return enif_make_uint64(env, rpmtdGetNumber(tag_data));

        case RPM_STRING_CLASS:
            return _string_to_binary(env, rpmtdGetString(tag_data));

        case RPM_BINARY_CLASS:
            return _binary_to_binary(env, tag_data->data, tag_data->count);

        default:
            return enif_make_atom(env, "unknown");
    }
}

static ERL_NIF_TERM _ecrepo_lib_error(ErlNifEnv *env, const char *error) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

static ERL_NIF_TERM _ecrepo_lib_ok(ErlNifEnv *env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static int *_list_to_list(ErlNifEnv *env, ERL_NIF_TERM list, int items, ERL_NIF_TERM *error) {
    int *result = (int *)calloc(items, sizeof(int));
    int i;
    ERL_NIF_TERM head;

    if (result == NULL) {
        *error = _ecrepo_lib_error(env, NOMEMORY);

        return result;
    }

    for (i = 0; i < items; ++i) {
        if (!enif_get_list_cell(env, list, &head, &list)) {
            /* In theory, this should never happend */
            free(result);

            *error = enif_make_badarg(env);

            return NULL;
        }

        int value;

        if (!enif_get_int(env, head, &value)) {
            free(result);

            *error = enif_make_badarg(env);

            return NULL;
        }

        result[i] = value;
    }

    return result;
}

static ERL_NIF_TERM _string_to_binary(ErlNifEnv *env, const char *string) {
    return _binary_to_binary(env, string, strlen(string));
}

static ERL_NIF_TERM _binary_to_binary(ErlNifEnv *env, const void *data, size_t size) {
    ErlNifBinary binary;

    if (!enif_alloc_binary(size, &binary)) {
        return _ecrepo_lib_error(env, NOMEMORY);
    }

    memcpy(binary.data, data, size);

    return enif_make_binary(env, &binary);
}
/*
}}}
 */
