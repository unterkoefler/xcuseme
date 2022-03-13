

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


ALTER TABLE public.events DISABLE TRIGGER ALL;

INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('c65fa6a1-52f5-4a5f-9646-62e2aa37babc', 'exercise', 'boutta go for a run soon :P', '2022-01-20', '2022-01-28 16:40:15.208875-05', '2022-01-28 16:40:15.208875-05', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('a572732e-7ff9-4234-807e-0c72f7800d65', 'excuse', 'Sleepy time', '2022-03-13', '2022-03-13 14:25:35.528457-04', '2022-03-13 14:25:35.528457-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('5102722d-91c3-4d01-9563-bcb2d4fde7ed', 'exercise', 'Went for run :D', '2022-03-12', '2022-03-13 14:25:49.519914-04', '2022-03-13 14:25:49.519914-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('26925409-f33e-4ae1-a417-181457f18a1a', 'exercise', 'Bike rideto work and back. Pretty nice day out, but colder in the evening. Still had to wear big gloves. ', '2022-03-11', '2022-03-13 14:26:35.618343-04', '2022-03-13 14:26:35.618343-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('601cfaf9-f5dc-41f3-954e-a47a1fd72069', 'excuse', 'I am a disappointment to myself and others', '2022-03-10', '2022-03-13 14:27:00.392527-04', '2022-03-13 14:27:00.392527-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('86a6c8b7-6a3f-4251-b6f3-54ff6fd4ea9a', 'exercise', 'Did some yoga in the evening before bed. Very relaxing', '2022-03-09', '2022-03-13 14:27:23.796456-04', '2022-03-13 14:27:23.796456-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('586e7510-c7a6-411c-96f2-ce8fb8fe8256', 'excuse', 'Not much hbu', '2022-03-08', '2022-03-13 15:10:31.893991-04', '2022-03-13 15:10:31.893991-04', '048129e8-4263-46bc-bc79-17bd2792062a');


ALTER TABLE public.events ENABLE TRIGGER ALL;


