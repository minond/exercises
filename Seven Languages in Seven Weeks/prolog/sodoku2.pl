% sodoku4x4([
%   1, 4, 2, _,
%   _, _, _, _,
%   _, _, _, _,
%   _, 3, _, 2
% ], Solution).

% sodoku9x9([
%   5, 4, _, 3, _, _, _, 2, 1,
%   _, _, _, _, 8, _, _, _, _,
%   1, _, _, _, 4, _, 5, _, _,
%   _, 2, 6, 4, 7, _, _, 5, _,
%   _, _, 8, _, _, _, 7, _, 2,
%   _, _, _, 1, _, _, _, _, 3,
%   _, 6, _, 9, 1, _, _, _, _,
%   8, _, _, _, 6, _, _, 3, _,
%   4, _, 2, _, _, 5, 6, 9, 7
% ], Solution).

valid([]).
valid([H|T]) :-
  fd_all_different(H),
  valid(T).

sodoku4x4(Program, Solution) :-
  Solution = Program,

  Program = [C11, C21, C31, C41,
             C12, C22, C32, C42,
             C13, C23, C33, C43,
             C14, C24, C34, C44],

  fd_domain(Solution, 1, 4),

  Cell1 = [C11, C21, C12, C22],
  Cell2 = [C31, C41, C32, C42],
  Cell3 = [C13, C23, C14, C24],
  Cell4 = [C33, C43, C34, C44],

  Col1 = [C11, C12, C13, C14],
  Col2 = [C21, C22, C23, C24],
  Col3 = [C31, C32, C33, C34],
  Col4 = [C41, C42, C43, C44],

  Row1 = [C11, C21, C31, C41],
  Row2 = [C12, C22, C32, C42],
  Row3 = [C13, C23, C33, C43],
  Row4 = [C14, C24, C34, C44],

  valid([Cell1, Cell2, Cell3, Cell4,
         Col1, Col2, Col3, Col4,
         Row1, Row2, Row3, Row4]).

sodoku9x9(Program, Solution) :-
  Solution = Program,

  Program = [C11, C21, C31, C41, C51, C61, C71, C81, C91,
             C12, C22, C32, C42, C52, C62, C72, C82, C92,
             C13, C23, C33, C43, C53, C63, C73, C83, C93,
             C14, C24, C34, C44, C54, C64, C74, C84, C94,
             C15, C25, C35, C45, C55, C65, C75, C85, C95,
             C16, C26, C36, C46, C56, C66, C76, C86, C96,
             C17, C27, C37, C47, C57, C67, C77, C87, C97,
             C18, C28, C38, C48, C58, C68, C78, C88, C98,
             C19, C29, C39, C49, C59, C69, C79, C89, C99],

  fd_domain(Program, 1, 9),

  Cell1 = [C11, C21, C31, C12, C22, C32, C13, C23, C33],
  Cell2 = [C41, C51, C61, C42, C52, C62, C43, C53, C63],
  Cell3 = [C71, C81, C91, C72, C82, C92, C73, C83, C93],
  Cell4 = [C14, C24, C34, C15, C25, C35, C16, C26, C36],
  Cell5 = [C44, C54, C64, C45, C55, C65, C46, C56, C66],
  Cell6 = [C74, C84, C94, C75, C85, C95, C76, C86, C96],
  Cell7 = [C17, C27, C37, C18, C28, C38, C19, C29, C39],
  Cell8 = [C47, C57, C67, C48, C58, C68, C49, C59, C69],
  Cell9 = [C77, C87, C97, C78, C88, C98, C79, C89, C99],

  Col1 = [C11, C12, C13, C14, C15, C16, C17, C18, C19],
  Col2 = [C21, C22, C23, C24, C25, C26, C27, C28, C29],
  Col3 = [C31, C32, C33, C34, C35, C36, C37, C38, C39],
  Col4 = [C41, C42, C43, C44, C45, C46, C47, C48, C49],
  Col5 = [C51, C52, C53, C54, C55, C56, C57, C58, C59],
  Col6 = [C61, C62, C63, C64, C65, C66, C67, C68, C69],
  Col7 = [C71, C72, C73, C74, C75, C76, C77, C78, C79],
  Col8 = [C81, C82, C83, C84, C85, C86, C87, C88, C89],
  Col9 = [C91, C92, C93, C94, C95, C96, C97, C98, C99],

  Row1 = [C11, C21, C31, C41, C51, C61, C71, C81, C91],
  Row2 = [C12, C22, C32, C42, C52, C62, C72, C82, C92],
  Row3 = [C13, C23, C33, C43, C53, C63, C73, C83, C93],
  Row4 = [C14, C24, C34, C44, C54, C64, C74, C84, C94],
  Row5 = [C15, C25, C35, C45, C55, C65, C75, C85, C95],
  Row6 = [C16, C26, C36, C46, C56, C66, C76, C86, C96],
  Row7 = [C17, C27, C37, C47, C57, C67, C77, C87, C97],
  Row8 = [C18, C28, C38, C48, C58, C68, C78, C88, C98],
  Row9 = [C19, C29, C39, C49, C59, C69, C79, C89, C99],

  valid([Cell1, Cell2, Cell3, Cell4, Cell5, Cell6, Cell7, Cell8, Cell9,
         Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9,
         Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9]).
