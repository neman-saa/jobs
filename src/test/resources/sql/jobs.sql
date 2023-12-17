CREATE DATABASE board;

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

 INSERT INTO jobs(
 id,
 date,
 ownerEmail,
 active,
 company,
 title,
 description,
 externalUrl,
 remote,
 location,
 salaryLo,
 salaryHi,
 currency,
 country,
 image,
 tags,
 seniority,
 other
 ) VALUES (
 '843df718-ec6e-4d49-9289-f799c0f40064',
 1659186086,
 'daniel@rockthejvm.com',
 true,
 'Awesome Company',
 'Tech Lead',
 'An awesome job in Berlin',
 'https://rockthejvm.com/awesomejob',
 false,
 'Berlin',
 2000,
 3000,
 'EUR',
 'Germany',
 NULL,
 ARRAY ['scala', 'scala-3', 'cats'],
 'Senior',
 NULL
 );