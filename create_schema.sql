
CREATE TABLE url (
    url_id BIGSERIAL PRIMARY KEY,
    -- this blog says TEXT and VARCHAR are stored the same under the hood, and TEXT has no max len: http://blog.jonanin.com/2013/11/20/postgresql-char-varchar/
    url    TEXT NOT NULL
);

-- I'm assuming that non-unique URL entries shouldn't be allowed
CREATE UNIQUE INDEX url_unique_idx ON url ( url );

CREATE TABLE url_access (
    url_access_id BIGSERIAL PRIMARY KEY,
    url_id      BIGINT NOT NULL REFERENCES url(url_id),
    datetime    TIMESTAMP NOT NULL,
    ip_address  VARCHAR(39) NOT NULL -- 39 chars max for IPv6
);
