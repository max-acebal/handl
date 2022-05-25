#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// #define LEN(arr) ((int) (sizeof (arr) / sizeof (arr)[0]))
/*
struct Note
{
    char pitch[4];
};
 
struct Phrase
{

    struct Note notes[8];
};

struct Song
{
    struct Phrase measures[32];
    // char timeSignature[4];
    // int bars;
};
*/
typedef char* Note;
typedef Note* Phrase;
typedef Phrase* Song;


 
int play_song(Song s1) 
{
    // int num_notes = (*s1).timeSignature[0] - '0';
    int num_notes = 8;
    int num_phrases = 8;
    // for (int i = 0; i < 32; i++) {
    //     for (int j = 0; j < 8; j++) {
    //         fprintf(stderr, "%s\n", s1[i][j]);
    //         //fprintf(stderr, "%s\n", (*s1)[i][j]);
    //     }
    // }
    for(int i=0; i < num_phrases; i++){
        
        
        
        int str_len = num_notes*3 + 2;
        
    
        char E[str_len];
        char A[str_len];
        char D[str_len];
        char G[str_len];
        char B[str_len];
        char e[str_len];
        
        E[0] = 'E';
        A[0] = 'A';
        D[0] = 'D';
        G[0] = 'G';
        B[0] = 'B';
        e[0] = 'e';
        
        E[1] = '|';
        A[1] = '|';
        D[1] = '|';
        G[1] = '|';
        B[1] = '|';
        e[1] = '|';
        
        //populate with dashes
        for(int n = 2; n < str_len; n++){
            E[n] = '-';
            A[n] = '-';
            D[n] = '-';
            G[n] = '-';
            B[n] = '-';
            e[n] = '-';
        }

        for(int j=0; j < num_notes; j++){
            // printf("%s", s1.measures[i].notes[j].pitch);
            // printf("\n");

            char* cur = (s1)[i][j];

            //High e string
            if(strcmp(cur,"E3")==0){
                e[3*j + 3] = '0';
                
            }
            else if(strcmp(cur,"F3")==0){
                e[3*j + 3] = '1';
            }
            else if(strcmp(cur,"F#3")==0 || strcmp(cur,"Gb3")==0){
                e[3*j + 3] = '2';
            }
            else if(strcmp(cur,"G3")==0){
                e[3*j + 3] = '3';
            }
            else if(strcmp(cur,"G#3")==0 || strcmp(cur,"Ab3")==0){
                e[3*j + 3] = '4';
            }
            else if(strcmp(cur,"A3")==0){
                e[3*j + 3] = '5';
            }
            else if(strcmp(cur,"Bb3")==0 || strcmp(cur,"A#3")==0){
                e[3*j + 3] = '6';
            }
            else if(strcmp(cur,"B3")==0){
                e[3*j + 3] = '7';
            }
            
            //B String
            else if(strcmp(cur,"B2")==0){
                B[3*j + 3] = '0';
            }
            else if(strcmp(cur,"C2")==0){
                B[3*j + 3] = '1';
            }
            else if(strcmp(cur,"C#2")==0 || strcmp(cur,"Db2")==0){
                B[3*j + 3] = '2';
            }
            else if(strcmp(cur,"D2")==0){
                B[3*j + 3] = '3';
            }
            else if(strcmp(cur,"D#2")==0 || strcmp(cur,"Eb2")==0){
                B[3*j + 3] = '4';
            }
            //G String
            else if(strcmp(cur,"G2")==0){
                G[3*j + 3] = '0';
            }
            else if(strcmp(cur,"G#2")==0 || strcmp(cur,"Ab2")==0){
                G[3*j + 3] = '1';
            }
            else if(strcmp(cur,"A2")==0 ){
                G[3*j + 3] = '2';
            }
            else if(strcmp(cur,"A#2")==0 || strcmp(cur,"Bb2")==0){
                G[3*j + 3] = '3';
            }
            else if(strcmp(cur,"B2")==0){
                G[3*j + 3] = '4';
            }
            //D string
            else if(strcmp(cur,"D1")==0){
                D[3*j + 3] = '0';
            }
            else if(strcmp(cur,"D#1")==0 || strcmp(cur,"Eb1")==0){
                D[3*j + 3] = '1';
            }
            else if(strcmp(cur,"E2")==0 ){
                D[3*j + 3] = '2';
            }
            else if(strcmp(cur,"F2")==0){
                D[3*j + 3] = '3';
            }
            else if(strcmp(cur,"F#2")==0 || strcmp(cur,"Gb2")==0){
                D[3*j + 3] = '4';
            }
            //A string
            else if(strcmp(cur,"A1")==0){
                A[3*j + 3] = '0';
            }
            else if(strcmp(cur,"A#1")==0 || strcmp(cur,"Bb1")==0){
                A[3*j + 3] = '1';
            }
            else if(strcmp(cur,"B1")==0 ){
                A[3*j + 3] = '2';
            }
            else if(strcmp(cur,"C1")==0){
                A[3*j + 3] = '3';
            }
            else if(strcmp(cur,"C#1")==0 || strcmp(cur,"Db1")==0){
                A[3*j + 3] = '4';
            }
            //Low E string
            else if(strcmp(cur,"E1")==0){
                E[3*j + 3] = '0';
                
            }
            else if(strcmp(cur,"F1")==0){
                E[3*j + 3] = '1';
            }
            else if(strcmp(cur,"F#1")==0 || strcmp(cur,"Gb1")==0){
                E[3*j + 3] = '2';
            }
            else if(strcmp(cur,"G1")==0){
                E[3*j + 3] = '3';
            }
            else if(strcmp(cur,"G#1")==0 || strcmp(cur,"Ab1")==0){
                E[3*j + 3] = '4';
            }
            
        }
        printf("%s\n",e);
        printf("%s\n",B);
        printf("%s\n",G);
        printf("%s\n",D);
        printf("%s\n",A);
        printf("%s\n",E);
        printf("//////////////////////////\n");

       
    }
 
    return 0;
}

// int main(){
//     struct Note n1 = {"E3"};
//     struct Note n2 = {"A2"};
//     struct Note n3 = {"A2"};
//     struct Note n4 = {"A1"};
    
//     struct Note n5 = {"B1"};
//     struct Note n6 = {"B2"};
//     struct Note n7 = {"A#2"};
//     struct Note n8 = {"D2"};
    
//     struct Phrase p1 = {{n1,n2,n3,n4,n1}};
//     struct Phrase p2 = {{n5,n6,n7,n8,n1}};
//     struct Phrase p3 = {{n1,n2,n3,n4,n1}};
//     struct Phrase p4 = {{n5,n6,n7,n8,n1}};
//     struct Phrase p5 = {{n1,n2,n3,n4,n1}};
//     struct Phrase p6 = {{n6,n2,n3,n4,n1}};
//     // struct Song s1 = {{p1,p2,p3,p4,p5},"4/4",5};
//     struct Song s1 = {{p1,p2,p3,p4,p5}};
    
//     play_song(&s1);
    
//     return 0;
// }

