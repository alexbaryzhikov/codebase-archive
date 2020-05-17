class Vector {
   public:
    Vector(int n);
    ~Vector();
    double &operator[](int i);
    int size();

   private:
    double *elements;
    int n;
};
