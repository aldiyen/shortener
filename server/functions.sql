-- Function to get information about a given URL by its PK
CREATE OR REPLACE FUNCTION load_url_info(requested_pk BIGINT)
    RETURNS TABLE (
        url_pk BIGINT,
        url TEXT,
        user_login_pk BIGINT,
        creator_username TEXT,
        created_datetime TIMESTAMPTZ,
        creator_ip_address TEXT,
        last_access_datetime TIMESTAMPTZ
    )
AS $$
SELECT u.url_pk,
       u.url,
       ul.user_login_pk,
       ul.username,
       uc.datetime,
       uc.ip_address,
       MAX(ua.datetime)
FROM url u
JOIN url_create uc ON uc.url_pk = u.url_pk
JOIN user_login ul ON ul.user_login_pk = uc.user_login_pk
LEFT JOIN url_access ua ON ua.url_pk = u.url_pk
WHERE u.url_pk = requested_pk
GROUP BY u.url_pk,
         u.url,
         uc.datetime,
         ul.user_login_pk,
         ul.username,
         uc.ip_address;
$$ LANGUAGE SQL STABLE;

-- Function to add a new URL, returning either the new URL PK or, in the case of a uniqueness conflict, the existing URL's PK
CREATE OR REPLACE FUNCTION add_url(new_url TEXT, created_datetime TIMESTAMPTZ, creator_ip_address VARCHAR(39), creator_user_login_pk BIGINT)
    RETURNS TABLE (
        url_pk BIGINT,
        was_created BOOLEAN
    )
AS $$
DECLARE new_pk BIGINT;
BEGIN
    CREATE TEMPORARY TABLE result (url_pk BIGINT, was_created BOOLEAN) ON COMMIT DROP;
    BEGIN
        INSERT INTO url (url) VALUES (new_url) RETURNING url.url_pk INTO new_pk;
        INSERT INTO url_create (url_pk, datetime, ip_address, user_login_pk) VALUES (new_pk, created_datetime, creator_ip_address, creator_user_login_pk);
        INSERT INTO result (url_pk, was_created) VALUES (new_pk, TRUE);
    EXCEPTION WHEN unique_violation THEN
        INSERT INTO result (url_pk, was_created) (
            SELECT url.url_pk, FALSE
            FROM url
            WHERE url = new_url
        );
    END;
    RETURN QUERY
        SELECT * FROM result;
END;
$$ LANGUAGE plpgsql VOLATILE;
