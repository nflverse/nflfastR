#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector decode_ids_cpp(std::vector< std::string > new_id){
  int n = new_id.size();
  CharacterVector out(n);
  for(int j = 0; j < n; ++j) {
    std::string id = new_id[j];
    if (id.size() == 36){
      std::string cut = id.substr(4, 24);
      std::string hex = "";
      for (int k = 0; k < cut.length(); ++k) {
        if (cut.substr(k, 1) == "-") {
          // hex = hex;
        } else {
          hex += cut.substr(k, 1);
        }
      }
      std::string ascii = "";
      for (size_t i = 0; i < hex.length(); i += 2){
        //taking two characters from hex string
        std::string part = hex.substr(i, 2);
        //changing it into base 16
        char ch = std::stoul(part, nullptr, 16);
        //putting it into the ASCII string
        ascii += ch;
      }
      out[j] = ascii;
    } else if (id.length() > 5) {
      out[j] = id;
    } else {
      out[j] = NA_STRING;
    }
  }
  return out;
}
