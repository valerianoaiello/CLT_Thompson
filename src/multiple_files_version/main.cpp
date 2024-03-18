#include <iostream>
#include "TreeDiagram.h"

int main() {
    TreeDiagram x_0 = TreeDiagram::createX0();
    TreeDiagram g = TreeDiagram::leftShiftHomomorphism(x_0);

    std::cout << "x_0: " << std::endl;
    x_0.printTreeDiagram();

    std::cout << "g: " << std::endl;
    g.printTreeDiagram();

    TreeDiagram x_1 = TreeDiagram::rightShiftHomomorphism(x_0);

    return 0;
}

    /*
    
    TreeDiagram prodotto;
    vector<int> formanormale;

    prodotto = multiplicationTreeDiagrams(x_1, x_0);
    formanormale = find_normal_form(prodotto).first;
    cout<< "x1x0\n";
    prodotto.printTreeDiagram();
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout<<endl;

    prodotto = x_1;
    cout<< "x1\n";
    prodotto.printTreeDiagram();
    formanormale = find_normal_form(prodotto).first;
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout<<endl;

    prodotto = x_0;
    formanormale = find_normal_form(prodotto).first;
    cout<< "x0\n";

    prodotto.printTreeDiagram();
    
    for (int i=0; i< formanormale.size(); ++i){
        cout<< " " << formanormale[i];
    }
    cout << endl;
    int d = 4;
    cout << "CIAO \n";
    for (int n=1; n<5; ++n){
        cout << unnormalized_moment(n,d) << ", ";
    }

    cout << endl;
    */
