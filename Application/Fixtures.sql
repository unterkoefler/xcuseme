

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

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('67426498-10b6-4c10-825f-8f58181984f0', 'wemil21@gmail.com', 'sha256|17|0p5QabecT5ZsX1iqLPG/uA==|ekSbKV1nRxIOM0Hf/ZW8dOT91/I36/twdlhJZTj5TNk=', NULL, 0);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('bba7e290-ebf3-489c-8cb7-a8d7c2f60e9d', 'example2@example.com', 'sha256|17|H5bYoIUPlzbq3rbn343ZzQ==|uhr745rT995c/5KjX9nDoRuAm6lGlNisedBzg2YMn4c=', NULL, 0);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('6670d3a1-d1a3-473e-a882-7205d034932b', 'example3@example.com', 'sha256|17|FQqFXMO8ju3TdPyyl6A75Q==|epWXCuTcH7/m4SAA4LGhmp4QXku2Ly5+cyOMhuZ2Y5c=', NULL, 0);
INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('fb9b82b4-e83f-4052-8495-b906912eb9f1', 'example4@example.com', 'sha256|17|LbG/4nGmjV26xv8esRetXw==|wfvi4Vjt6BIxKXH15r4BwhkqnCTjNPLpHOTgYNq78wo=', NULL, 0);
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
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('850468f5-fa35-445c-bf27-eacce56c8bff', 'excuse', 'chance of rain in the forecast', '2022-03-17', '2022-03-17 21:02:47.131744-04', '2022-03-17 21:02:47.131744-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('ecda6644-2955-45e8-9e3b-e651e3d0ec4d', 'exercise', 'hi world', '2022-03-04', '2022-03-17 21:06:16.271784-04', '2022-03-17 21:06:16.271784-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('3a80ad43-95aa-418f-b645-06cc1710871f', 'exercise', 'Just doing a thing', '2022-04-17', '2022-04-17 14:06:05.582974-04', '2022-04-17 14:06:05.582974-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('33428dd7-6430-4c51-86d9-ae1a2180c82c', 'excuse', 'Nope', '2022-04-10', '2022-04-17 14:06:29.221286-04', '2022-04-17 14:06:29.221286-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('5438041a-3c8c-4547-9135-80dc0b173806', 'exercise', 'yoo', '2022-04-11', '2022-04-17 15:05:06.383946-04', '2022-04-17 15:05:06.383946-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('0b10dc89-e1fa-404b-891f-cc2adf49cf9b', 'exercise', 'fogojgk', '2022-04-12', '2022-04-17 15:08:31.133389-04', '2022-04-17 15:08:31.133389-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('b25346bd-9a8c-448d-87be-a0e7b3bcfea6', 'exercise', 'fkln', '2022-04-13', '2022-04-17 15:22:30.868219-04', '2022-04-17 15:22:30.868219-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('b946f2f2-26f7-40b4-bde6-b5586b11f467', 'exercise', 'ahhhhhh', '2022-04-14', '2022-04-17 17:44:11.145221-04', '2022-04-17 17:44:11.145221-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('4cad44a8-75e9-4950-8c9f-ce29c93340e1', 'exercise', 'idk  bro', '2022-04-15', '2022-04-17 18:24:44.402256-04', '2022-04-17 18:24:44.402256-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('ca225f65-89aa-4dc9-85e5-eb84a492cd72', 'excuse', 'National excuse day', '2022-04-20', '2022-04-30 00:22:45.165008-04', '2022-04-30 00:22:45.165008-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('8c7d895d-bec2-4133-8619-9de1603201ed', 'exercise', 'hjhj', '2022-05-01', '2022-04-30 14:48:27.521653-04', '2022-04-30 14:48:27.521653-04', 'fb9b82b4-e83f-4052-8495-b906912eb9f1');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('bd5e8621-03a6-4b7b-bbef-e8d5ec9bc16c', 'exercise', 'Test', '2022-05-08', '2022-05-08 20:23:31.088468-04', '2022-05-08 20:23:31.088468-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('19ac380c-e73b-44c8-a05e-b744b7921244', 'exercise', 'Did thing', '2022-05-04', '2022-05-08 20:30:41.38587-04', '2022-05-08 20:30:41.38587-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('1ce6d1c7-b607-4381-9416-219169901ef3', 'exercise', 'test testtest', '2022-05-05', '2022-05-08 20:32:03.852565-04', '2022-05-08 20:32:03.852565-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('f6e47526-ce49-466a-8b6e-dbaf6cea5caa', 'exercise', 'nm ', '2022-05-09', '2022-05-10 12:14:06.131429-04', '2022-05-10 12:14:06.131429-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('71172d25-556d-4bde-bb61-b31037a04ec3', 'excuse', 'an excuse', '2022-05-07', '2022-05-10 12:14:52.277188-04', '2022-05-10 12:14:52.277188-04', '048129e8-4263-46bc-bc79-17bd2792062a');
INSERT INTO public.events (id, event_type, description, date, created_at, updated_at, user_id) VALUES ('044cfb9f-ea9d-4e48-8449-92b564769a17', 'excuse', 'booo', '2022-05-10', '2022-05-10 12:15:59.076687-04', '2022-05-10 12:15:59.076687-04', '048129e8-4263-46bc-bc79-17bd2792062a');


ALTER TABLE public.events ENABLE TRIGGER ALL;


