grammar Alchemist2;

molecule
    : mainChain ;

mainChain
    : ramification* carbonChainWithSuffix 
    | ramification* SPECIAL_SUFFIX
    | ramification* 'benzene' ;

carbonChain
    : 'cyclo'? RADICAL ;

carbonChainWithSuffix
    : carbonChain suffix* ;

carbonChainWithRamifications
    : ramification* carbonChain ;

// note that most of the prefixes and alkyl ramifications
// are similar wrt to their format

prefix 
    : PREFIX                      // both can be elided in case there is only one function group
    | positions MULTIPLIER PREFIX // if we have positions, we must have the corresponding multipler
    | MULTIPLIER PREFIX           // positions can be elided
    | positions PREFIX ;          // a single positions

// PREFIXES / RAMIFICATIONS

// -fluoro
// -cloro
// -bromo
// -iodo
// -hydroxy 
// -mercapto 
// -imino
// -oxo (never on extreme)
// -formyl
// -carboxy
// -amido
// -phenyl

// -([subramification])? amino          <-- this
// -([subramification])? phosphino      <-- this
// -([subramification])? arsino         <-- and this, these 3 can hold subramifications
// -([subramification])? (CARBON CHAIN) yl
// -                     (CARBON CHAIN) oxy         <-- ethers
// -                     (CARBON CHAIN) oxycarbonyl <-- esters
// -                     (CARBON CHAIN) oyloxy      <-- also esters

// note that all 4 combinations are present
// therefore, we need to handle them!

// SUFFIXES

// -ol
// thiol
// imine
// one
// al
// oic acid
// carboxylic acid
// amide

/* move all of this into MAIN CHAIN

// (ALKYL)* amine     
// (ALKYL)* phosphine
// (ALKYL)* arsine              <-- these 3 can be used directly on the main chain, or used normally
// (ALKYL)+ ether               <-- can ONLY be used directly on the main chain
// (ALKYL) ' ' (mainChain) oate <-- most difficult one to parse
*/

// CARBON CHAIN
// 'cyclo' radical (CARBON_SUFFIX)*

// MAIN CHAIN
// CARBON CHAIN
// benzene <-- this is a special case
// amine
// phosphine
// arsine
// ether
// ' ' RAMIFICATION* (CARBON CHAIN) oate

// MOLECULE
// RAMIFICATION* (MAIN CHAIN)

suffix
    : positions? MULTIPLIER? (SUFFIX | CARBON_CHAIN_SUFFIX) ;

ramification
    : positions? MULTIPLIER? PREFIX
    | positions? MULTIPLIER? subramification? carbonChainWithSuffix 'yl' ;

subramification
    : '[' ramification ']' ;

positions
    : NUMBER (',' NUMBER)* ;

ester  
    : ramification ' ' mainChain 'oate' ;

RADICAL
    : 'undec'
    | 'dodec'
    | 'tridec'
    | 'tetradec'
    | 'pentadec'
    | 'hexadec'
    | 'heptadec'
    | 'octadec'
    | 'nonadec'
    | 'meth'
    | 'eth'
    | 'prop'
    | 'but'
    | 'pent'
    | 'hex'
    | 'hept'
    | 'oct'
    | 'non'
    | 'dec' ;

MULTIPLIER
    : 'undeca'
    | 'dodeca'
    | 'trideca'
    | 'tetradeca'
    | 'pentadeca'
    | 'hexadeca'
    | 'heptadeca'
    | 'octadeca'
    | 'nonadeca' 
    | 'di'
    | 'tri'
    | 'tetra'
    | 'penta'
    | 'hexa'
    | 'hepta'
    | 'octa'
    | 'nona'
    | 'deca' ;  
    // this is context sensitive
    // we probably need a parse that can backtrack and try different alternative

HALOGEN
    : 'fluoro'
    | 'chloro'
    | 'bromo'
    | 'iodo' ;

PREFIX
    : HALOGEN
    | 'hydroxy'
    | 'mercapto'
    | 'imino'
    | 'oxo'
    | 'formyl' 
    | 'carboxy'
    | 'amido'
    | 'amino'
    | 'phosphino'
    | 'arsino' 
    | 'phenyl' ;

SPECIAL_SUFFIX
    : 'amine'
    | 'phosphine'
    | 'arsine' 
    | 'either' ;

SUFFIX
    : 'ol' 
    | 'thiol'
    | 'imine'
    | 'one'                 // cannot be at extremity
    | 'al'                  // only at extremity 
    | 'oic acid'            // only at extremity
    | 'carboxylic acid'     // only at extremity
    | 'amide'               // only at extremity 
    | SPECIAL_SUFFIX ; 

CARBON_CHAIN_SUFFIX
    : 'en'
    | 'yn' 
    | 'an' ;

NUMBER: [0-9]+ ;

DASH: '-' -> skip ;
