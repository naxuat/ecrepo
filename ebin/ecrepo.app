{application, ecrepo, [
    {description, "RPM Repository Creator in Erlang"},
    {vsn, "0.0.0"},
    {applications, [
        kernel,
        stdlib
    ]},
    {modules, [
        ecrepo,
        ecrepo_cli,
        ecrepo_getopt,
        ecrepo_indices,
        ecrepo_lib,
        ecrepo_utils,
        ecrepo_xml
    ]},
    {registered, [
    ]},
    {env, [
    ]}
]}.
