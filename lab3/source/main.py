from PIL import Image
from pylab import *
import scipy.stats as st
import scipy.signal as sign
import numpy as np

file_path = '../resources/'
IMG1_NAME = 'img1.jpg'
IMG2_NAME = 'img2.jpg'


def create_grayscale_img(img_name):

    def transform_img(img_name):
        image = Image.open(file_path + img_name).convert('L')  # Открываем изображение и конвертируем в полутоновое

        return image

    image = transform_img(img_name)
    image.save(file_path + "grayscale_" + img_name)
    return image


def analize_img(image):
    hist_data = array(image).flatten()
    stat = {}

    stat.update([
        ('mean', np.mean(hist_data)),
        ('std', np.std(hist_data)),
        ('mode', st.mode(hist_data)[0][0]),
        ('median', np.median(hist_data))])

    return hist_data, stat


def img_histogram(image):

    def show_histogram(data, bins):
        figure()
        hist(data, bins)
        show()

    img_array = array(image).flatten()
    show_histogram(img_array, 26)


def img_correlation(img1, img2):
    x = np.array(img1)
    y = np.array(img2)

    return sign.correlate2d(x, y)

def hyp_check_ks(_dataset):
    print(st.kstest(_dataset, cdf='norm'))

def hyp_check_chi2(_dataset):
    chi2, p = st.chisquare(_dataset)
    msg = "Test Statistic: {}\np-value: {}"
    print(msg.format(chi2, p))

def main():
    image1 = create_grayscale_img(IMG1_NAME)
    image2 = create_grayscale_img(IMG2_NAME)

    img_histogram(image1)
    img_histogram(image2)

    hist1, hist_stat1 = analize_img(image1)
    hist2, hist_stat2 = analize_img(image2)

    print('Explore:')
    print("First image: ", hist_stat1)
    print("Second image: ", hist_stat2)

    print("Histogram correlation: ",
          np.corrcoef(
              np.asarray(hist1).flatten(),
              np.asarray(hist2).flatten()
          )[1, 0])

    print('Images` correlation', img_correlation(image1, image2))

    print('\nHypothesis:\n')
    hyp_check_chi2(hist1)
    hyp_check_chi2(hist2)

    hyp_check_ks(hist1)
    hyp_check_ks(hist2)


if __name__ == "__main__":
    main()
