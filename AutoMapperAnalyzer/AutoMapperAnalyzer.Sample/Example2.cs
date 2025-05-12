using AutoMapper;
using System.Collections.Generic;


namespace AutoMapperAnalyzer.Sample.too
{
    public class b : Profile
    {
        public b()
        {


            CreateMap<Examples.UserDto, Examples.UserEntity>()
                .ForMember(dest => dest.Namg, opt => opt.MapFrom(src => src.Name))
                .ReverseMap()
                .ForPath(desc => desc.Name, opt => opt.MapFrom(src => src.Namg));
            
        }
    }

    public class Examples
    {
        public class UserDto
        {
            public string Name { get; set; }

            public ICollection<Sample.Examples.C> a { get; set; }

        }

        public class UserEntity
        {
            public string Namg { get; set; }

            public ICollection<Sample.Examples.D> a { get; set; }
        }

        public class A
        {
            public string? FirstName { get; set; }

            public string LastName { get; set; }
        }

        public class B
        {
            public string FirstName { get; set; }

            public string LastName { get; set; }
        }

        public void ToStars()
        {


        }
    }
}
