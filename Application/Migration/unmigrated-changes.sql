-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
--
-- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
--
-- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html
CREATE TYPE event_types AS ENUM ('exercise', 'excuse');
CREATE TABLE events (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    event_type event_types NOT NULL,
    description TEXT NOT NULL,
    date DATE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    user_id UUID NOT NULL
);
CREATE INDEX events_user_id_index ON events (user_id);
ALTER TABLE events ADD CONSTRAINT events_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
CREATE TABLE carter (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
DROP TABLE carter;
CREATE TABLE books (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    fjdj TEXT NOT NULL
);
DROP TABLE books;
