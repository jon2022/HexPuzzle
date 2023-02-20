//
//  main.cpp
//  hexPuzzle
//
//  Created by Jonathan Michaels on 19/02/2023.
//

#include <iostream>


using namespace std;

int main() {

    const int tileSet[7][6]=  //define a set of seven tiles (one solution)
    {   {1, 2, 3, 4, 5, 6},
        {1, 3, 6, 4, 5, 2},
        {1, 6, 2, 5, 3, 4},
        {1, 2, 5, 6, 3, 4},
        {1, 2, 3, 4, 6, 5},
        {1, 4, 3, 5, 2, 6},
        {1, 5, 3, 6, 2, 4}
    };
//
//    const int tileSet[7][6]=  //define a set of seven tiles (seven solutions)
//    {   {1, 2, 3, 4, 5, 6},
//        {3, 1, 2, 4, 5, 6},
//        {4, 2, 3, 5, 6, 1},
//        {5, 3, 4, 6, 1, 2},
//        {6, 4, 5, 1, 2, 3},
//        {1, 5, 6, 2, 3, 4},
//        {2, 6, 1, 3, 4, 5}
//    };
//



//    const int tileSet[7][6]=  //define a set of seven tiles (two solutions
//    {   {1, 2, 3, 4, 5, 6},
//        {1, 3, 2, 6, 5, 4},
//        {1, 6, 3, 5, 2, 4},
//        {1, 6, 3, 5, 2, 4},
//        {1, 4, 6, 5, 3, 2},
//        {1, 4, 3, 6, 2, 5},
//        {1, 5, 3, 6, 2, 4}
//    };
        
//    const int tileSet[7][6]=  //define a set of seven tiles (no solutions
//    {   {1, 2, 3, 4, 5, 6},
//        {1, 3, 2, 6, 5, 4},
//        {1, 6, 3, 5, 2, 4},
//        {1, 6, 3, 4, 2, 5},
//        {1, 4, 6, 5, 3, 2},
//        {1, 4, 3, 6, 2, 5},
//        {1, 5, 3, 6, 2, 4}
//    };



    int tileOrder[7] = {0,1,2,3,4,5,6};  //variable for tile order 0 to 6 around clock, 7 in centre
    int prevSide[6]; //variable for previous and
    int nextSide[6];  //next side that need to match
    int countAnswers=0;  // variable to count correct solutions

    
    
    // main programme creates all possible orders of 7 tiles,
    // tests for working solutions and
    // print out working orders and count of solutions
    for(tileOrder[0]=0;tileOrder[0]<7;tileOrder[0]++){
        for(tileOrder[1]=0;tileOrder[1]<7;tileOrder[1]++){
            if(tileOrder[1]==tileOrder[0]){
                continue;
            }
            for(tileOrder[2]=0;tileOrder[2]<7;tileOrder[2]++){
                if(tileOrder[2]==tileOrder[0]|tileOrder[2]==tileOrder[1]){
                    continue;

                }
                for(tileOrder[3]=0;tileOrder[3]<7;tileOrder[3]++){
                    if(tileOrder[3]==tileOrder[0]|tileOrder[3]==tileOrder[1]|tileOrder[3]==tileOrder[2]){
                        continue;

                    }
                    for(tileOrder[4]=0;tileOrder[4]<7;tileOrder[4]++){
                        if(tileOrder[4]==tileOrder[0]|tileOrder[4]==tileOrder[1]|tileOrder[4]==tileOrder[2]|tileOrder[4]==tileOrder[3]){
                            continue;

                        }for(tileOrder[5]=0;tileOrder[5]<7;tileOrder[5]++){
                            if(tileOrder[5]==tileOrder[0]|tileOrder[5]==tileOrder[1]|tileOrder[5]==tileOrder[2]|tileOrder[5]==tileOrder[3]|tileOrder[5]==tileOrder[4]){
                                continue;
                            }for(tileOrder[6]=0;tileOrder[6]<7;tileOrder[6]++){
                                if(tileOrder[6]==tileOrder[0]|tileOrder[6]==tileOrder[1]|tileOrder[6]==tileOrder[2]|tileOrder[6]==tileOrder[3]|tileOrder[6]==tileOrder[4]|tileOrder[6]==tileOrder[5]){
                                    continue;


                                }
                                
                                for (int i=0;i<6;i++){ //loop through each tile in position 0 to 5
                                    for (int j=0;j<6;j++){  //loop through each side of the tile
                                        if (tileSet[tileOrder[i]][j]==tileSet[tileOrder[6]][i]){ //if the side matches the side in the centre tile
                                            prevSide[i]=tileSet[tileOrder[i]][(j+5)%6];
                                            nextSide[i]=tileSet[tileOrder[i]][(j+1)%6];
                                        }
                                    }
                                }
                                if(prevSide[0]==nextSide[1]&prevSide[1]==nextSide[2]&prevSide[2]==nextSide[3]&prevSide[3]==nextSide[4]&prevSide[4]==nextSide[5]&prevSide[5]==nextSide[0]){
                                    cout<<"\nWorking solution"<<"\n";
                                    cout<<tileOrder[0]<<" "<<tileOrder[1]<<" "<<tileOrder[2]<<" "<<tileOrder[3]<<" "<<tileOrder[4]<<" "<<tileOrder[5]<<" "<<tileOrder[6]<<"\n\n";
                                    countAnswers++;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    cout<<countAnswers<<" correct solutions identified\nCheck complete\n\n";
}
