-- create schema checklist2;
-- set schema 'checklist2';

create sequence checklist2.command_id_seq;

create table checklist2.commands (
  cid int not null default nextval('checklist2.command_id_seq') primary key,
  username text not null,
  updated timestamp with time zone not null default current_timestamp,
  cmddata text not null
);

create index commands_cid_idx ON checklist2.commands ((cmddata :: json ->> 'employecid'));
