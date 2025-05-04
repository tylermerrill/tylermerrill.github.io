DROP TABLE IF EXISTS animal;
DROP TABLE IF EXISTS own;
DROP TABLE IF EXISTS staff;
DROP TABLE IF EXISTS vital;
DROP TABLE IF EXISTS appointment;
DROP TABLE IF EXISTS animal_vet;
DROP TABLE IF EXISTS owner_vet;

CREATE TABLE own (
  o_id VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  phone VARCHAR(255) NOT NULL,
  address VARCHAR(255) NOT NULL,
  PRIMARY KEY (o_id)
);
CREATE TABLE staff (
  s_id VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  salary INTEGER NOT NULL,
  position VARCHAR(255) NOT NULL,
  PRIMARY KEY (s_id)
);
CREATE TABLE animal (
  aid VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  age INTEGER NOT NULL,
  breed VARCHAR(255),
  sex VARCHAR(255) NOT NULL,
  species VARCHAR(255) NOT NULL,
  o_id VARCHAR(255) NOT NULL,
  PRIMARY KEY (aid),
  FOREIGN KEY (o_id) REFERENCES own(o_id)
);
CREATE TABLE vital (
  r_id VARCHAR(255) NOT NULL,
  weight NUMERIC(5,2),
  rbc NUMERIC(5,2),
  wbc NUMERIC(5,2),
  respiration NUMERIC(5,2),
  ph NUMERIC(5,2),
  pulse NUMERIC(5,2),
  glucose NUMERIC(5,2),
  s_id VARCHAR(255) NOT NULL,
  aid VARCHAR(255) NOT NULL,
  PRIMARY KEY (r_id),
  FOREIGN KEY (s_id) REFERENCES staff(s_id),
  FOREIGN KEY (aid) REFERENCES animal(aid)
);
CREATE TABLE appointment (
  appointment_date DATE NOT NULL,
  appointment_time VARCHAR NOT NULL,
  reason VARCHAR(255),
  ontime VARCHAR(255),
  s_id VARCHAR(255) NOT NULL,
  aid VARCHAR(255) NOT NULL,
  o_id VARCHAR(255) NOT NULL,
  r_id VARCHAR(255) NOT NULL,
  PRIMARY KEY (appointment_date, appointment_time),
  FOREIGN KEY (s_id) REFERENCES staff(s_id),
  FOREIGN KEY (aid) REFERENCES animal(aid),
  FOREIGN KEY (o_id) REFERENCES own(o_id),
  FOREIGN KEY (r_id) REFERENCES vital(r_id)
);
CREATE TABLE animal_vet (
  aid VARCHAR(255) NOT NULL,
  s_id VARCHAR(255) NOT NULL,
  PRIMARY KEY (aid, s_id),
  FOREIGN KEY (aid) REFERENCES animal(aid),
  FOREIGN KEY (s_id) REFERENCES staff(s_id)
);
CREATE TABLE owner_vet (
  o_id VARCHAR(255) NOT NULL,
  s_id VARCHAR(255) NOT NULL,
  PRIMARY KEY (o_id, s_id),
  FOREIGN KEY (o_id) REFERENCES own(o_id),
  FOREIGN KEY (s_id) REFERENCES staff(s_id)
);
INSERT INTO own (o_id, name, phone, address) VALUES
('44a7c', 'Kathy Renyolds', '972-555-8322', '1818 Cherrywood Ln'),
('21b5x', 'Norman Bates', '972-761-4324', '6537 Edgemund Dr'),
('57c4b', 'Katie Chun', '972-443-6981', '1324 Park Rd'),
('123ab', 'Alice Smith', '214-555-1212', '4516 Oak Ave'),
('789cd', 'Bob Johnson', '817-555-3434', '7789 Pine St'),
('345ef', 'Charlie Brown', '903-555-5656', '1223 Elm St'),
('901gh', 'Diana Lee', '469-555-7878', '4566 Maple Dr'),
('567ij', 'Ethan Davis', '214-555-9090', '7689 Willow Ln'),
('234kl', 'Fiona Green', '817-555-1111', '1253 Oak St');
INSERT INTO staff (s_id, name, salary, position) VALUES
('aa67b', 'Devin Hernandez', 88000, 'Vet'),
('qc46e', 'Cindy Yang', 102000, 'Vet'),
('wx91z', 'Micheal Douwitz', 66000, 'Vet'),
('yu88l', 'Dorothy Gale', 180000, 'Vet'),
('er49u', 'Stacy Jones', 137000, 'Vet'),
('ez44b', 'Monica Adams', 40000, 'Receptionist');
INSERT INTO animal (aid, name, age, breed, sex, species, o_id) VALUES
('a3b76c', 'Fluffy', 7, 'German Sherpard', 'F', 'Dog', '44a7c'),
('f3g66j', 'Max', 2, 'Bull', 'M', 'Dog', '21b5x'),
('c3h34h', 'Tom', 6, 'Simieese', 'M', 'Cat', '57c4b'),
('q2s35c', 'Tim', 9, 'Brazilian', 'M', 'Parrot', '57c4b'),
('b4c87d', 'Buddy', 3, 'Labrador', 'M', 'Dog', '44a7c'),
('g4h77k', 'Bella', 5, 'Golden Retriever', 'F', 'Dog', '21b5x'),
('d4e45i', 'Whiskers', 1, 'Siamese', 'M', 'Cat', '57c4b'),
('r3t66d', 'Polly', 10, 'Macaw', 'F', 'Bird', '123ab'),
('w2q55a', 'Rocky', 4, 'Boxer', 'M', 'Dog', '789cd'),
('e5r88t', 'Lucy', 2, 'Poodle', 'F', 'Dog', '345ef'),
('t6y99b', 'Oliver', 7, 'Maine Coon', 'M', 'Cat', '345ef'),
('i9o88c', 'Luna', 3, '', 'F', 'Snake', '901gh'),
('p1a44z', 'Daisy', 1, 'Ragdoll', 'F', 'Cat', '567ij'),
('a2s77w', 'Rio', 9, 'African Grey', 'M', 'Bird', '234kl'),
('s3d00q', 'Tucker', 2, '', 'M', 'Hamster', '234kl');
INSERT INTO vital (r_id, weight, rbc, wbc, respiration, ph, pulse, glucose, s_id, aid) VALUES
('442341', 23.0, 8.7, 6.12, 19, 6.9, 80, 90, 'aa67b', 'a3b76c'),
('322341', 22.7, 8.6, 6.14, 19, 6.9, 80, 90, 'wx91z', 'a3b76c'),
('678932', 9.08, 6.1, 7.1, 24, 6.2, 93, 88, 'wx91z', 'c3h34h'),
('734598', 1.2, 3.7, 109, 14, 7.9, 212, 300, 'qc46e', 'q2s35c'),
('111abc', 25.5, 7.8, 20, 7.1, 75, 95, 95, 'aa67b', 'a3b76c'),
('222def', 30.2, 8.1, 18, 7.0, 82, 88, 96, 'qc46e', 'f3g66j'),
('333ghi', 4.1, 6.5, 25, 7.2, 110, 105, 97, 'qc46e', 'c3h34h'),
('555mno', 28.7, 7.9, 19, 7.1, 78, 92, 86,  'yu88l', 'b4c87d'),
('666pqr', 26.1, 8.0, 21, 6.9, 85, 86, 81, 'er49u', 'g4h77k'),
('777stu', 5.3, 6.8, 23, 7.3, 105, 108, 83, 'aa67b', 'd4e45i');
INSERT INTO appointment (appointment_date, appointment_time, reason, ontime, s_id, aid, o_id, r_id) VALUES
('2024-01-12', '0637am', 'Vomit', 'Yes', 'aa67b', 'a3b76c', '44a7c','442341'),
('2024-04-19', '0230pm', 'Tired', 'Yes', 'wx91z', 'a3b76c', '44a7c', '322341'),
('2025-04-13', '1130am', 'Check-Up', 'No', 'wx91z', 'c3h34h', '57c4b', '678932'),
('2025-04-12', '0930am', 'Check-Up', 'Yes', 'qc46e', 'q2s35c', '57c4b', '734598'),
('2025-03-12', '1030am', 'Overactive', 'No', 'aa67b', 'a3b76c', '44a7c', '111abc'),
('2025-06-23', '0130pm', 'Tired', 'Yes', 'qc46e', 'f3g66j', '21b5x', '222def'),
('2025-07-19', '0330pm', 'Limping', 'Yes', 'qc46e', 'c3h34h', '57c4b', '333ghi'),
('2025-07-11', '0430pm', 'Cough', 'Yes', 'yu88l', 'b4c87d', '44a7c', '555mno'),
('2025-08-08', '0115pm', 'Appetite Loss', 'Yes', 'er49u', 'g4h77k', '21b5x', '666pqr'),
('2025-09-28', '0117pm', 'Nail Trim', 'Yes', 'aa67b', 'd4e45i', '57c4b', '777stu');