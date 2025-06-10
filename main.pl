/*****

CONTROLS

*****/

    /**

    MISC

    **/
    
        /*
        purpose: show available commands
        returns: list of commands
        */
        help :-
        write('Available commands:'), nl,
        write('Movement:'), nl,
        write('  move(Direction)  - Move up/down/left/right'), nl,
        write('  where           - Show current position'), nl,
        write('  map            - Show detailed room info'), nl, nl,
        write('Inventory:'), nl,
        write('  pickup(Object)  - Pick up an adjacent object'), nl,
        write('  place(Object,Direction) - Place an object in a direction'), nl,
        write('  inv            - Show inventory contents'), nl.

    /**

    MOVEMENT

    **/

        /*
        purpose: show where you are
        returns: room, (x,y) coordinates
        */
        :- dynamic where/3.

        /* helper to not have to ask with args */
        where :-
            where(Room, X, Y),
            write('You are in '), write(Room), write(' at '), write([X,Y]), nl.

        /*
        purpose: give full detail of the current room
        returns: room, (x,y) coordinates, room's size, doorways, objects
        */
        map :-
            where(Room, X, Y),
            room(Room, W, H),
            write('You are in '), write(Room), 
            write(' at '), write([X,Y]), nl,
            write('Room size: '), write(W), write('x'), write(H), nl,
            write('Doorways:'), nl,
            forall(doorway_exists(Room, NextRoom, DX, DY, _, _, Dir),
                (write('  - To '), write(NextRoom), 
                write(' at '), write([DX,DY]),
                write(' going '), write(Dir), nl)
            ),
            write('Objects:'), nl,
            forall(object_located(Obj, Room, OX, OY, OZ),
                (write('  - '), write(Obj), 
                write(' at '), write([OX,OY,OZ]), nl)
            ).

        /*
        purpose: move in a cardinal direction
        returns: true if moved, false if blocked or out of bounds
        */
        move(X) :-
            direction(X, Dx, Dy),
            where(Room, CurrX, CurrY),
            room(Room, Rx, Ry),
            NewX is CurrX + Dx,
            NewY is CurrY + Dy,
            (doorway_exists(Room, Room2, CurrX, CurrY, DestX, DestY, X) ->
                retract(where(Room, CurrX, CurrY)),
                assert(where(Room2, DestX, DestY)),
                write('Moved through doorway to '), write(Room2), write(' at '), write(DestX), write(','), write(DestY), nl ;
                ((NewX < 0 ; NewX >= Rx ; NewY < 0 ; NewY >= Ry) ->
                    write('Cannot move outside the room'), fail
                ;
                    (space_taken(Room, NewX, NewY, _) -> 
                        write('Blocked by object'), fail
                    ;
                        retract(where(Room, CurrX, CurrY)),
                        assert(where(Room, NewX, NewY)),
                        write('Moved '), write(X), write(' to '), write(Room), write(' at '), write(NewX), write(','), write(NewY), nl
                    )
                )
            ),
            (doorway_exists(Room, Room2, NewX, NewY, _, _, Dir) -> write('There is a doorway to '), write(Room2), write(' to the '), write(Dir), nl ; true),
            !.

    /**

    INVENTORY

    **/

        /*
        purpose: user's inventory, used to store objects
        returns: list of objects in the inventory
        */
        :- dynamic inventory/1.

        inventory([]).

        /* Max inventory size */
        inventory_capacity(3).


        /* Pickup an object (has to be near you in either cardinal direction */
        pickup(Object) :-
            where(Room, X, Y),
            object_located(Object, Room, ObjX, ObjY, _),
            adjacent(X, Y, ObjX, ObjY),
            inventory(Current),
            inventory_capacity(Cap),
            length(Current, Len),
            (Len >= Cap ->
                write('Inventory full!'), nl, fail
            ;
                retract(object_located(Object, Room, ObjX, ObjY, _)),
                retract(inventory(Current)),
                append(Current, [Object], NewInv),
                assert(inventory(NewInv)),
                write('Picked up '), write(Object), nl
            ), !.

        /* Place an object 1 space around you. */
        place(Object, Direction) :-
            inventory(Current),
            member(Object, Current),
            where(Room, X, Y),
            direction(Direction, Dx, Dy),
            NewX is X + Dx,
            NewY is Y + Dy,
            room(Room, Rx, Ry),
            (NewX < 0 ; NewX >= Rx ; NewY < 0 ; NewY >= Ry ->
                write('Cannot place outside the room'), fail
            ;
                (space_taken(Room, NewX, NewY, _) -> 
                    write('Space taken by another object'), fail
                ;
                    retract(inventory(Current)),
                    delete(Current, Object, NewInv),
                    assert(inventory(NewInv)),
                    assert(object_located(Object, Room, NewX, NewY, 0)),
                    write('Placed '), write(Object), 
                    write(' '), write(Direction), nl
                )
            ), !.

        /* Helper to see inventory */
        inv :-
            inventory(Items),
            write('Inventory: '), write(Items), nl.



/*****

    STRUCTURES

*****/

    /*
    * purpose: define objects in the world.
    * params: 
        Name - name of the object
    */
    object(hanger).
    object(sofa).
    object(tv).
    object(drawer).
    object(toilet).
    object(sink).
    object(shower).
    object(bed).
    object(desk).
    object(chair).
    object(fridge).
    object(oven).
    object(pc).
    object(table).

    /*
    purpose: define rooms in the  world.
    params: 
        Name - name
        X - x coordinate of the room
        Y - y coordinate of the room
    */
    room(hallway, 2, 5).      % Central corridor
    room(livingroom, 5, 4).   % Left side
    room(bathroom, 3, 3).     % Upper side
    room(bedroom, 4, 4).      % Lower right
    room(kitchen, 4, 3).      % Upper right

    /*
    purpose: define doorways between rooms.
    params:
        Room1 - name of the first room,
        Room2 - name of the second room,
        X1 - door x coord in the first room
        Y1 - door y coord in the first room
        X2 - door x coord in the second room
        Y2 - door y coord in the second room
        direction - which way to go from room1 to reach room2
    */
    doorway(hallway, livingroom, 0, 2, 4, 2, left).    % Left to living room
    doorway(hallway, bathroom, 1, 0, 1, 2, up).        % Up to bathroom
    doorway(hallway, kitchen, 1, 1, 0, 2, right).      % Right to kitchen
    doorway(hallway, bedroom, 1, 4, 0, 2, right).      % Right to bedroom


/*****

INNER LOGIC

*****/

    /*
    purpose: define cardinal directions, used for movement and placing objects
    returns: direction, change 
    */
    direction(up, 0, -1).
    direction(down, 0, 1).
    direction(right, 1, 0).
    direction(left, -1, 0).


    /*
    purpose: check if two coordinates are adjacent
    re  turns: true if adjacent, false otherwise
    */
    adjacent(X1, Y1, X2, Y2) :-
        abs(X1 - X2) =< 1,
        abs(Y1 - Y2) =< 1.

    /*
    purpose: represents an association between an object and its location. objects can share at most two coordinates in a given room.
    */
    :- dynamic object_located/5.

    /*
    purpose: checks if a space is occupied by an object
    returns: true if occupied, false if free
    */
    space_taken(Room, X, Y, Z) :-
        object_located(_, Room, X, Y, Z).


    /*
    purpose: check if an object is being correctly placed
    returns: true if placement is valid, false otherwise
    */
    object_setup(Object, Room, X, Y, Z) :-
        object(Object),
        room(Room, Rx, Ry),
        integer(X), integer(Y), integer(Z),
        Rx >= X, X >= 0, Ry >= Y, Y >= 0, Z >= 0,
        /* check if placement is valid (on floor or on top of something) */
        (Z = 0 -> 
            true 
        ; 
            (Z > 0, Z1 is Z-1, 
            (space_taken(Room, X, Y, Z1) -> 
                true
            ;
                write('Cannot place object in mid-air!'), nl, fail
            ))
        ),
        /* Check if target space is free */
        (space_taken(Room, X, Y, Z) -> 
            write('Space taken by another object.'), nl, fail
        ; 
            assert(object_located(Object, Room, X, Y, Z)),
            write('Placed '), write(Object), write(' at '), write([X,Y,Z]), write(' in '), write(Room), nl
        ), !.

    /*
    purpose: invert doorway direction if second room is first
    */
    invert_direction(up, down).
    invert_direction(down, up).
    invert_direction(left, right).
    invert_direction(right, left).

    /*
    purpose: check if a doorway exists between two rooms (bidirectional)
    returns: true if doorway exists, false otherwise
    */
    doorway_exists(Room1, Room2, X1, Y1, X2, Y2, Direction) :-
        doorway(Room1, Room2, X1, Y1, X2, Y2, Direction).
    doorway_exists(Room1, Room2, X1, Y1, X2, Y2, InvertedDirection) :-
        doorway(Room2, Room1, X2, Y2, X1, Y1, Direction),
        invert_direction(Direction, InvertedDirection).



/*****

    SPATIAL RELATIONS

*****/

/*
purpose: check if object1 is ontop of object2
returns: true if object1 is on top of object2, false otherwise 
*/
on(Object1, Object2) :-
    object_located(Object1, Room, X, Y, Z1),
    object_located(Object2, Room, X, Y, Z2),
    Z1 > Z2.

/*
check if object1 is adjacent to object2
returns: true if object1 is next to object2, false otherwise
*/
next_to(Object1, Object2) :-
    object_located(Object1, Room, X1, Y1, _),
    object_located(Object2, Room, X2, Y2, _),
    adjacent(X1, Y1, X2, Y2),
    Object1 \= Object2.

/*
purpose: check if an object is between two other objects
returns: true if object is between object1 and object2, false otherwise
*/
between(Object1, Object, Object2) :-
    object_located(Object, Room, X, Y, _),
    object_located(Object1, Room, X1, Y1, _),
    object_located(Object2, Room, X2, Y2, _),
    X >= min(X1, X2),
    X =< max(X1, X2),
    Y >= min(Y1, Y2),
    Y =< max(Y1, Y2),
    Object \= Object1,
    Object \= Object2.

/*****

    PATHFINDING

*****/

:- dynamic current_path/1.
current_path([]).



/*
purpose: find path from current position to target object, assumes you have to see the object first. It treats all objects as fully opaque and without height.
returns: a list of directions to follow to reach the object
*/
find_path(Object) :-
    where(CurrentRoom, UserX, UserY),
    object_located(Object, ObjRoom, ObjX, ObjY, _),
    
    % Check if in same room
    (CurrentRoom \= ObjRoom -> 
        write('Cannot find path to '), write(Object), 
        write(' - not in the same room!'), nl,
        fail
    ;
        /* Check if object is already adjacent */
        (adjacent(UserX, UserY, ObjX, ObjY) ->
            write(Object), write(' is already adjacent!'), nl,
            assert(current_path([]))
        ;
            /* Try finding path */
            retractall(current_path(_)),
            (try_path(CurrentRoom, UserX, UserY, ObjX, ObjY, [], Path) ->
                assert(current_path(Path)),
                write('Path found: '), write(Path), nl
            ;
                write('Couldn\'t find path to '), write(Object), nl,
                fail
        ))), !.

/* Calculate directions (horizontal, vertical) */
get_directions(UserX, UserY, ObjX, ObjY, DirX, DirY) :-
    DiffX is ObjX - UserX,
    DiffY is ObjY - UserY,
    (DiffX > 0 -> DirX = right; DirX = left),
    (DiffY > 0 -> DirY = down; DirY = up).

/* Base case - reached the object */
try_path(_, UserX, UserY, ObjX, ObjY, Path, Path) :-
    adjacent(UserX, UserY, ObjX, ObjY), !.

/* Recursive case */
try_path(Room, UserX, UserY, ObjX, ObjY, CurrentPath, FinalPath) :-
    /* Get movement directions */
    DiffX is ObjX - UserX,
    DiffY is ObjY - UserY,
    get_directions(UserX, UserY, ObjX, ObjY, DirX, DirY),

    /* Try either X then Y, or Y then X */
    (
        /* Try X first */
        (DiffX \= 0,
         try_move(Room, UserX, UserY, DirX, NewX, NewY) ->
            try_path(Room, NewX, NewY, ObjX, ObjY, [DirX|CurrentPath], FinalPath)
        ;
            /* Blocked - try Y */   
            (DiffY \= 0,
             try_move(Room, UserX, UserY, DirY, NewX, NewY) ->
                try_path(Room, NewX, NewY, ObjX, ObjY, [DirY|CurrentPath], FinalPath)
            ;
                fail
            )
        )
    ;
        /* Try Y first */
        DiffY \= 0,
         try_move(Room, UserX, UserY, DirY, NewX, NewY) ->
            try_path(Room, NewX, NewY, ObjX, ObjY, [DirY|CurrentPath], FinalPath)
        ;
            /* Blocked - try X */
            (DiffX \= 0,
             try_move(Room, UserX, UserY, DirX, NewX, NewY) ->
                try_path(Room, NewX, NewY, ObjX, ObjY, [DirX|CurrentPath], FinalPath)
            ;
                fail
            )
        ).

/* Perform a fake move */
try_move(Room, X, Y, Dir, NewX, NewY) :-
    direction(Dir, DX, DY),
    NewX is X + DX,
    NewY is Y + DY,
    room(Room, RoomW, RoomH),
    NewX >= 0, NewX < RoomW,
    NewY >= 0, NewY < RoomH,
    \+ space_taken(Room, NewX, NewY, _).


/*****

    OBJECT SETUP

*****/

clear_objects :-
    retractall(object_located(_,_,_,_,_)).

setup_objects :-
    clear_objects,
    /* Hallway objects */
    object_setup(hanger, hallway, 0, 0, 0),
    
    /* Living room setup */
    object_setup(sofa, livingroom, 2, 1, 0),
    object_setup(table, livingroom, 2, 0, 0),
    object_setup(tv, livingroom, 2, 0, 1),
    object_setup(drawer, livingroom, 0, 0, 0),
    
    /* Bathroom setup */
    object_setup(toilet, bathroom, 0, 0, 0),
    object_setup(sink, bathroom, 1, 0, 0),
    object_setup(shower, bathroom, 2, 0, 0),
    
    /* Bedroom setup */
    object_setup(bed, bedroom, 1, 1, 0),
    object_setup(desk, bedroom, 0, 0, 0),
    object_setup(pc, bedroom, 0, 0, 1),
    object_setup(chair, bedroom, 0, 1, 0),
    
    /* Kitchen setup */
    object_setup(fridge, kitchen, 3, 0, 0),
    object_setup(oven, kitchen, 2, 0, 0),
    
    write('Objects placed successfully!'), nl.

init :-
    retractall(where(_,_,_)),          % Clear any existing position
    retractall(inventory(_)),          % Clear inventory
    assert(inventory([])),             % Set empty inventory
    assert(where(hallway, 1, 2)),      % Set initial position
    setup_objects,                     % Place objects
    write('World initialized! Type "map." to see your surroundings.'), nl.
