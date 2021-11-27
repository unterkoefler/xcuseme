-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE event_types AS ENUM ('exercise', 'excuse');
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE UNIQUE INDEX users_email_index ON users (LOWER(email));
CREATE TABLE events (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    event_type event_types NOT NULL,
    description TEXT NOT NULL,
    date DATE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    user_id UUID NOT NULL
);
CREATE UNIQUE INDEX events_unique_by_date ON events (date, user_id);
CREATE INDEX events_user_id_index ON events (user_id);
ALTER TABLE events ADD CONSTRAINT events_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
