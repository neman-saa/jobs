CREATE DATABASE board;
\c board;

 CREATE TABLE jobs(
     id uuid DEFAULT gen_random_uuid(),
     date bigint NOT NULL,
     ownerEmail text NOT NULL,
     active boolean NOT NULL DEFAULT false,
     company text NOT NULL,
     title text NOT NULL,
     description text NOT NULL,
     externalUrl text NOT NULL,
     remote boolean NOT NULL DEFAULT false,
     location text,
     salaryLo integer,
     salaryHi integer,
     currency text,
     country text,
     image text,
     tags text[],
     seniority text,
     other text
 );

 ALTER TABLE jobs
 ADD CONSTRAINT pk_jobs PRIMARY KEY (id);

 CREATE TABLE users (
 email text NOT NULL,
 hashedPassword text NOT NULL,
 firstName text,
 lastName text,
 company text,
 role text NOT NULL
 );

 ALTER TABLE users
 ADD CONSTRAINT pk_users PRIMARY KEY(email);
