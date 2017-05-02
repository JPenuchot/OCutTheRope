open Graphics

let transpcol = rgb 18 52 86

let image_from_ppm n = 
    let e = open_in_bin n in
    let is_bw = (input_line e <> "P6") in
    let w,h = 
        let l = ref (input_line e) in
        while !l.[0] = '#' do l := input_line e done ;
        Scanf.sscanf !l "%d %d" (fun x y -> x,y)
        in
        let _ = input_line e in
        let m = Array.make_matrix h w (rgb 0 0 0) in
        for i = 0 to h - 1 do
            for j = 0 to w - 1 do
                let r, g, b =
                    if is_bw  then
                        let x = input_byte e in 
                        x, x, x
                    else
                        let r = input_byte e in
                        let g = input_byte e in
                        let b = input_byte e in
                        r, g, b
                        in
                        m.(i).(j) <- if rgb r g b == transpcol then
                                        transp
                                    else
                                        rgb r g b
            done
            done ;
    close_in e ;
    make_image m