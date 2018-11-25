template <class>
struct get_template_arguments;

template <template <class...> class Class, class... Args>
struct get_template_arguments<Class<Args...>> {
    using type = std::tuple<Args...>;
};

template <class T>
using get_template_arguments_t = typename get_template_arguments<T>::type;
