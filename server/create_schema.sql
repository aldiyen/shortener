CREATE TABLE user_login (
    user_login_pk         BIGSERIAL   PRIMARY KEY,
    username              TEXT        NOT NULL,
    password              TEXT        NOT NULL,
    creation_datetime     TIMESTAMPTZ NOT NULL,
    creator_user_login_pk BIGINT      NULL REFERENCES user_login(user_login_pk)
);

CREATE UNIQUE INDEX user_login_unique_username ON user_login (username);

CREATE TABLE url (
    url_pk            BIGSERIAL   PRIMARY KEY,
    url               TEXT        NOT NULL,
    user_login_pk     BIGINT      NOT NULL REFERENCES user_login(user_login_pk),
    creation_datetime TIMESTAMPTZ NOT NULL,
    creator_ip        VARCHAR(39) NOT NULL,
    UNIQUE (url, user_login_pk)
);

CREATE INDEX url_url_idx ON url ( url );
CREATE INDEX url_user_login_pk_idx ON url ( user_login_pk );

CREATE TABLE url_access (
    url_access_pk BIGSERIAL   PRIMARY KEY,
    url_pk        BIGINT      NOT NULL REFERENCES url(url_pk),
    datetime      TIMESTAMPTZ NOT NULL,
    ip_address    VARCHAR(39) NOT NULL -- 39 chars max for IPv6
);

CREATE TABLE user_token (
    user_token_pk      BIGSERIAL   PRIMARY KEY,
    user_login_pk      BIGINT      NOT NULL REFERENCES user_login(user_login_pk),
    token              TEXT        NOT NULL,
    creation_datetime  TIMESTAMPTZ NOT NULL,
    creator_ip_address VARCHAR(39) NOT NULL
);

CREATE UNIQUE INDEX user_token_unique_token ON user_token (user_login_pk, token);
