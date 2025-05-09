using AutoMapper;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

            public Sample.Examples.C a { get; set; }

        }

        public class UserEntity
        {
            public string Namg { get; set; }

            public Sample.Examples.D a { get; set; }
        }

        public class A
        {
            public string FirstName { get; set; }

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
