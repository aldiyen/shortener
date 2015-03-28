
CREATE TABLE url (
    url_pk BIGSERIAL PRIMARY KEY,
    -- this blog says TEXT and VARCHAR are stored the same under the hood, but TEXT has no max len: http://blog.jonanin.com/2013/11/20/postgresql-char-varchar/
    url    TEXT NOT NULL
);

-- I'm assuming that non-unique URL entries shouldn't be allowed
CREATE UNIQUE INDEX url_unique_pkx ON url ( url );

CREATE TABLE url_access (
    url_access_pk BIGSERIAL   PRIMARY KEY,
    url_pk        BIGINT      NOT NULL REFERENCES url(url_pk),
    datetime      TIMESTAMPTZ NOT NULL,
    ip_address    VARCHAR(39) NOT NULL -- 39 chars max for IPv6
);

CREATE TABLE user_login (
    user_login_pk  BIGSERIAL PRIMARY KEY,
    username       TEXT      NOT NULL,
    password       TEXT      NOT NULL
);

CREATE UNIQUE INDEX user_login_unique_username ON user_login (username);

CREATE TABLE url_create (
    url_create_pk BIGSERIAL   PRIMARY KEY,
    url_pk        BIGINT      NOT NULL REFERENCES url(url_pk),
    datetime      TIMESTAMPTZ NOT NULL,
    ip_address    VARCHAR(39) NOT NULL, -- 39 chars max for IPv6
    user_login_pk BIGINT      NOT NULL REFERENCES user_login(user_login_pk),
    UNIQUE (url_pk)
);
