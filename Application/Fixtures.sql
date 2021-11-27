

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('048129e8-4263-46bc-bc79-17bd2792062a', 'example@example.com', 'sha256|17|Gu2VYeLk/KgJVgCzR8H2RA==|TL9q7gGekAOWAP6pSWSjdX29q0zupYutzasLcKX2Qb8=', NULL, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


